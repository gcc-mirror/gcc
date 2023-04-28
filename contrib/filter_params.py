#!/usr/bin/env python3
"""
Filters out some of the #defines used throughout the GCC sources:
- GTY(()) marks declarations for gengtype.c
- PARAMS(()) is used for K&R compatibility. See ansidecl.h.

When passed one or more filenames, acts on those files and prints the
results to stdout.

When run without a filename, runs a unit-testing suite.
"""
import re
import sys
import unittest

# Optional whitespace
OPT_WS = '\s*'

def filter_src(text):
    """
    str -> str.  We operate on the whole of the source file at once
    (rather than individual lines) so that we can have multiline
    regexes.
    """

    # Convert C comments from GNU coding convention of:
    #    /* FIRST_LINE
    #       NEXT_LINE
    #       FINAL_LINE.  */
    # to:
    #    /** @verbatim FIRST_LINE
    #       NEXT_LINE
    #       FINAL_LINE.  @endverbatim */
    # so that doxygen will parse them.
    #
    # Only comments that begin on the left-most column are converted.
    #
    text = re.sub(r'^/\*\* ',
                  r'/** @verbatim ',
                  text,
                  flags=re.MULTILINE)
    text = re.sub(r'^/\* ',
                  r'/** @verbatim ',
                  text,
                  flags=re.MULTILINE)
    text = re.sub(r'\*/',
                  r' @endverbatim */',
                  text)

    # Remove GTY markings (potentially multiline ones):
    text = re.sub('GTY' + OPT_WS + r'\(\(.*?\)\)\s+',
                  '',
                  text,
                  flags=(re.MULTILINE|re.DOTALL))

    # Strip out 'ATTRIBUTE_UNUSED'
    text = re.sub('\sATTRIBUTE_UNUSED',
                  '',
                  text)

    # PARAMS(()) is used for K&R compatibility. See ansidecl.h.
    text = re.sub('PARAMS' + OPT_WS + r'\(\((.*?)\)\)',
                  r'(\1)',
                  text)

    # Replace 'ENUM_BITFIELD(enum_name)' with 'enum enum_name'.
    text = re.sub('ENUM_BITFIELD\s*\(([^\)]*)\)',
                  r'enum \1',
                  text)

    return text

class FilteringTests(unittest.TestCase):
    '''
    Unit tests for filter_src.
    '''
    def assert_filters_to(self, src_input, expected_result):
        # assertMultiLineEqual was added to unittest in 2.7/3.1
        if hasattr(self, 'assertMultiLineEqual'):
            assertion = self.assertMultiLineEqual
        else:
            assertion = self.assertEqual
        assertion(expected_result, filter_src(src_input))

    def test_comment_example(self):
        self.assert_filters_to(
            ('/* FIRST_LINE\n'
             '   NEXT_LINE\n'
             '   FINAL_LINE.  */\n'),
            ('/** @verbatim FIRST_LINE\n'
             '   NEXT_LINE\n'
             '   FINAL_LINE.   @endverbatim */\n'))

    def test_comment_example_gengtype(self):
        self.assert_filters_to(
            ('/** Allocate and initialize an input buffer state.\n'
             ' * @param file A readable stream.\n'
             ' * @param size The character buffer size in bytes. When in doubt, use @c YY_BUF_SIZE.\n'
             ' * \n'
             ' * @return the allocated buffer state.\n'
             ' */'),
            ('/** @verbatim Allocate and initialize an input buffer state.\n'
             ' * @param file A readable stream.\n'
             ' * @param size The character buffer size in bytes. When in doubt, use @c YY_BUF_SIZE.\n'
             ' * \n'
             ' * @return the allocated buffer state.\n'
             '  @endverbatim */'))

    def test_oneliner_comment(self):
        self.assert_filters_to(
            '/* Returns the string representing CLASS.  */\n',
            ('/** @verbatim Returns the string representing CLASS.   @endverbatim */\n'))

    def test_multiline_comment(self):
        self.assert_filters_to(
            ('/* The thread-local storage model associated with a given VAR_DECL\n'
             "   or SYMBOL_REF.  This isn't used much, but both trees and RTL refer\n"
             "   to it, so it's here.  */\n"),
            ('/** @verbatim The thread-local storage model associated with a given VAR_DECL\n'
             "   or SYMBOL_REF.  This isn't used much, but both trees and RTL refer\n"
             "   to it, so it's here.   @endverbatim */\n"))

    def test_GTY(self):
        self.assert_filters_to(
            ('typedef struct GTY(()) alias_pair {\n'
             '  tree decl;\n'
             '  tree target;\n'
             '} alias_pair;\n'),
            ('typedef struct alias_pair {\n'
             '  tree decl;\n'
             '  tree target;\n'
             '} alias_pair;\n'))

    def test_multiline_GTY(self):
        # Ensure that a multiline GTY is filtered out.
        self.assert_filters_to(
            ('class GTY((desc ("%h.type"), tag ("SYMTAB_SYMBOL"),\n'
             '\t   chain_next ("%h.next"), chain_prev ("%h.previous")))\n'
             '  symtab_node_base\n'
             '{\n'),
            ('class symtab_node_base\n'
             '{\n'))

    def test_ATTRIBUTE_UNUSED(self):
        # Ensure that ATTRIBUTE_UNUSED is filtered out.
        self.assert_filters_to(
            ('static void\n'
             'record_set (rtx dest, const_rtx set, void *data ATTRIBUTE_UNUSED)\n'
             '{\n'),
            ('static void\n'
             'record_set (rtx dest, const_rtx set, void *data)\n'
             '{\n'))

    def test_PARAMS(self):
        self.assert_filters_to(
            'char *strcpy PARAMS ((char *dest, char *source));\n',
            'char *strcpy (char *dest, char *source);\n')

    def test_ENUM_BITFIELD(self):
        self.assert_filters_to(
            '  ENUM_BITFIELD (sym_intent) intent:2;\n',
            '  enum sym_intent intent:2;\n')

def act_on_files(argv):
    for filename in argv[1:]:
        with open(filename) as f:
            text = f.read()
            print(filter_src(text))

if __name__ == '__main__':
    if len(sys.argv) > 1:
        act_on_files(sys.argv)
    else:
        unittest.main()
