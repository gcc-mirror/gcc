import json


# Parameters.
ALL_ERRORS = False


def _report_error(msg):
    '''Report an error.'''
    full_msg = 'ERROR: ' + msg
    if ALL_ERRORS:
        print(full_msg)
    else:
        raise RuntimeError(full_msg)


class Token(object):
    pass


class Output(Token):
    def __init__(self, path):
        self.path = path


class Input(Token):
    def __init__(self, path):
        self.path = path


class OrderInput(Token):
    def __init__(self, path):
        self.path = path


class Colon(Token):
    pass


class Append(Token):
    pass


class Variable(Token):
    def __init__(self, name):
        self.name = name


class Word(Token):
    def __init__(self, name):
        self.name = name


def validate_depfile(depfile, expect_input=None):
    '''Validate a depfile contains some information

    Returns `False` if the information is not found.
    '''
    with open(depfile, 'r') as fin:
        depfile_content = fin.read()

    real_lines = []
    join_line = False
    for line in depfile_content.split('\n'):
        # Join the line if needed.
        if join_line:
            line = real_lines.pop() + line

        # Detect line continuations.
        join_line = line.endswith('\\')
        # Strip line continuation characters.
        if join_line:
            line = line[:-1]

        # Add to the real line set.
        real_lines.append(line)

    # Perform tokenization.
    tokenized_lines = []
    for line in real_lines:
        tokenized = []
        join_word = False
        for word in line.split(' '):
            if join_word:
                word = tokenized.pop() + ' ' + word

            # Detect word joins.
            join_word = word.endswith('\\')
            # Strip escape character.
            if join_word:
                word = word[:-1]

            # Detect `:` at the end of a word.
            if word.endswith(':'):
                tokenized.append(word[:-1])
                word = word[-1]
            # Detect `:` at the end of a word.
            if word.endswith(':|'):
                tokenized.append(word[:-2])
                word = word[-2]

            # Add word to the tokenized set.
            tokenized.append(word)

        tokenized_lines.append(tokenized)

    # Parse.
    ast = []
    for line in tokenized_lines:
        kind = None
        for token in line:
            if token == ':':
                kind = 'dependency'
            elif token == '+=':
                kind = 'append'
            elif token == ':|':
                kind = 'order-only'
        if line == ['']:
            kind = 'empty'

        if kind is None:
            _report_error('unknown line kind: %s' % line)

        line_parse = []
        if kind == 'dependency':
            after_colon = False
            for token in line:
                if token == ':':
                    after_colon = True
                elif after_colon:
                    line_parse.append(Input(token))
                else:
                    line_parse.append(Output(token))
        elif kind == 'order-only':
            after_op = False
            for token in line:
                if token == ':|':
                    after_op = True
                elif after_op:
                    line_parse.append(OrderInput(token))
                else:
                    line_parse.append(Output(token))
        elif kind == 'append':
            after_op = False
            for token in line:
                if token == '+=':
                    after_op = True
                elif after_op:
                    line_parse.append(Word(token))
                else:
                    line_parse.append(Variable(token))

        ast.append(line_parse)

    for node in ast:
        for token in node:
            if expect_input is not None:
                # If the input is found, clear the expectation.
                if isinstance(token, Input) and token.path == expect_input:
                    expect_input = None

    result = True
    if expect_input:
        _report_error('missing input: %s' % expect_input)
        result = False

    return result


if __name__ == '__main__':
    import sys

    depfile = None
    have_expect = False
    expect_input = None

    # Parse arguments.
    args = sys.argv[1:]
    while args:
        # Take an argument.
        arg = args.pop(0)

        # Flag to change how errors are reported.
        if arg == '-A' or arg == '--all':
            ALL_ERRORS = True
        # Required arguments.
        elif arg == '-d' or arg == '--depfile':
            depfile = args.pop(0)
        elif arg == '-i' or arg == '--expect-input':
            expect_input = args.pop(0)
            have_expect = True

    # Validate that we have the required arguments.
    if depfile is None:
        raise RuntimeError('missing "depfile" file')
    if have_expect is None:
        raise RuntimeError('missing an "expect" argument')

    # Do the actual work.
    try:
        is_ok = validate_depfile(depfile, expect_input=expect_input)
    except BaseException as e:
        _report_error('exception: %s' % e)

    # Fail if errors are found.
    if not is_ok:
        sys.exit(1)
