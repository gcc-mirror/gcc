import json


# Parameters.
ALL_ERRORS = False
REPLACEMENTS = {}


def _print_path(path):
    '''Format a JSON path for output.'''
    return '/'.join(path)


def _report_error(msg):
    '''Report an error.'''
    full_msg = 'ERROR: ' + msg
    if ALL_ERRORS:
        print(full_msg)
    else:
        raise RuntimeError(full_msg)


def _error_type_mismatch(path, actual, expect):
    '''Report that there is a type mismatch.'''
    _report_error('type mismatch at %s: actual: "%s" expect: "%s"' % (_print_path(path), actual, expect))


def _error_unknown_type(path, typ):
    '''Report that there is an unknown type in the JSON object.'''
    _report_error('unknown type at %s: "%s"' % (_print_path(path), typ))


def _error_length_mismatch(path, actual, expect):
    '''Report a length mismatch in an object or array.'''
    _report_error('length mismatch at %s: actual: "%s" expect: "%s"' % (_print_path(path), actual, expect))


def _error_unexpect_value(path, actual, expect):
    '''Report a value mismatch.'''
    _report_error('value mismatch at %s: actual: "%s" expect: "%s"' % (_print_path(path), actual, expect))


def _error_extra_key(path, key):
    '''Report on a key that is unexpected.'''
    _report_error('extra key at %s: "%s"' % (_print_path(path), key))


def _error_missing_key(path, key):
    '''Report on a key that is missing.'''
    _report_error('extra key at %s: %s' % (_print_path(path), key))


def _compare_object(path, actual, expect):
    '''Compare a JSON object.'''
    is_ok = True

    if not len(actual) == len(expect):
        _error_length_mismatch(path, len(actual), len(expect))
        is_ok = False

    for key in actual:
        if key not in expect:
            _error_extra_key(path, key)
            is_ok = False
        else:
            sub_error = compare_json(path + [key], actual[key], expect[key])
            if sub_error:
                is_ok = False

    for key in expect:
        if key not in actual:
            _error_missing_key(path, key)
            is_ok = False

    return is_ok


def _compare_array(path, actual, expect):
    '''Compare a JSON array.'''
    is_ok = True

    if not len(actual) == len(expect):
        _error_length_mismatch(path, len(actual), len(expect))
        is_ok = False

    for (idx, (a, e)) in enumerate(zip(actual, expect)):
        sub_error = compare_json(path + [str(idx)], a, e)
        if sub_error:
            is_ok = False

    return is_ok


def _make_replacements(value):
    for (old, new) in REPLACEMENTS.values():
        value = value.replace(old, new)
    return value


def _compare_string(path, actual, expect):
    '''Compare a JSON string supporting replacements in the expected output.'''
    expect = _make_replacements(expect)

    if not actual == expect:
        _error_unexpect_value(path, actual, expect)
        return False
    else:
        print('%s is ok: %s' % (_print_path(path), actual))
    return True


def _compare_number(path, actual, expect):
    '''Compare a JSON integer.'''
    if not actual == expect:
        _error_unexpect_value(path, actual, expect)
        return False
    else:
        print('%s is ok: %s' % (_print_path(path), actual))
    return True


def _inspect_ordering(arr):
    req_ordering = True

    if not arr:
        return arr, req_ordering

    if arr[0] == '__P1689_unordered__':
        arr.pop(0)
        req_ordering = False

    return arr, req_ordering


def compare_json(path, actual, expect):
    actual_type = type(actual)
    expect_type = type(expect)

    is_ok = True

    if not actual_type == expect_type:
        _error_type_mismatch(path, actual_type, expect_type)
        is_ok = False
    elif actual_type == dict:
        is_ok = _compare_object(path, actual, expect)
    elif actual_type == list:
        expect, req_ordering = _inspect_ordering(expect)
        if not req_ordering:
            actual = set(actual)
            expect = set(expect)
        is_ok = _compare_array(path, actual, expect)
    elif actual_type == str:
        is_ok = _compare_string(path, actual, expect)
    elif actual_type == float:
        is_ok = _compare_number(path, actual, expect)
    elif actual_type == int:
        is_ok = _compare_number(path, actual, expect)
    elif actual_type == bool:
        is_ok = _compare_number(path, actual, expect)
    elif actual_type == type(None):
        pass
    else:
        _error_unknown_type(path, actual_type)
        is_ok = False

    return is_ok


def validate_p1689(actual, expect):
    '''Validate a P1689 file against an expected output file.

    Returns `False` if it fails, `True` if they are the same.
    '''
    with open(actual, 'r') as fin:
        actual_content = fin.read()
    with open(expect, 'r') as fin:
        expect_content = fin.read()

    actual_json = json.loads(actual_content)
    expect_json = json.loads(expect_content)

    return compare_json([], actual_json, expect_json)


if __name__ == '__main__':
    import sys

    actual = None
    expect = None

    # Parse arguments.
    args = sys.argv[1:]
    while args:
        # Take an argument.
        arg = args.pop(0)

        # Parse out replacement expressions.
        if arg == '-r' or arg == '--replace':
            replacement = args.pop(0)
            (key, value) = replacement.split('=', maxsplit=1)
            REPLACEMENTS[key] = value
        # Flag to change how errors are reported.
        elif arg == '-A' or arg == '--all':
            ALL_ERRORS = True
        # Required arguments.
        elif arg == '-a' or arg == '--actual':
            actual = args.pop(0)
        elif arg == '-e' or arg == '--expect':
            expect = args.pop(0)

    # Validate that we have the required arguments.
    if actual is None:
        raise RuntimeError('missing "actual" file')
    if expect is None:
        raise RuntimeError('missing "expect" file')

    # Do the actual work.
    is_ok = validate_p1689(actual, expect)

    # Fail if errors are found.
    if not is_ok:
        sys.exit(1)
