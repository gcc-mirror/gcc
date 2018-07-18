/* { dg-do compile } */
/* { dg-options "-O -fdiagnostics-show-caret" } */

/* This is a collection of unittests for ranges within string literals,
   using diagnostic_plugin_test_string_literals, which handles
   "__emit_string_literal_range" by generating a warning at the given
   subset of a string literal.

   The indices are 0-based.  It's easiest to verify things using string
   literals that are runs of 0-based digits (to avoid having to count
   characters).

   LITERAL is a const void * to allow testing the various kinds of wide
   string literal, rather than just const char *.  */

extern void __emit_string_literal_range (const void *literal, int caret_idx,
					 int start_idx, int end_idx);

void
test_simple_string_literal (void)
{
  __emit_string_literal_range ("0123456789", /* { dg-warning "range" } */
			       6, 6, 7);
/* { dg-begin-multiline-output "" }
   __emit_string_literal_range ("0123456789",
                                       ^~
   { dg-end-multiline-output "" } */
}

void
test_concatenated_string_literal (void)
{
  __emit_string_literal_range ("01234" "56789", /* { dg-warning "range" } */
			       4, 3, 6);
/* { dg-begin-multiline-output "" }
   __emit_string_literal_range ("01234" "56789",
                                    ~^~~~~~
   { dg-end-multiline-output "" } */
}

void
test_multiline_string_literal (void)
{
  __emit_string_literal_range ("01234" /* { dg-warning "range" } */
                               "56789",
                               4, 3, 6);
/* { dg-begin-multiline-output "" }
   __emit_string_literal_range ("01234"
                                    ~^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                "56789",
                                ~~~   
   { dg-end-multiline-output "" } */
  /* FIXME: why does the above need three trailing spaces?  */
}

/* Tests of various unicode encodings.

   Digits 0 through 9 are unicode code points:
      U+0030 DIGIT ZERO
      ...
      U+0039 DIGIT NINE
   However, these are not always valid as UCN (see the comment in
   libcpp/charset.c:_cpp_valid_ucn).

   Hence we need to test UCN using an alternative unicode
   representation of numbers; let's use Roman numerals,
   (though these start at one, not zero):
      U+2170 SMALL ROMAN NUMERAL ONE
      ...
      U+2174 SMALL ROMAN NUMERAL FIVE  ("v")
      U+2175 SMALL ROMAN NUMERAL SIX   ("vi")
      ...
      U+2178 SMALL ROMAN NUMERAL NINE.  */

void
test_hex (void)
{
  /* Digits 0-9, expressing digit 5 in ASCII as "\x35"
     and with a space in place of digit 6, to terminate the escaped
     hex code.  */
  __emit_string_literal_range ("01234\x35 789", /* { dg-warning "range" } */
			       4, 3, 7);
/* { dg-begin-multiline-output "" }
   __emit_string_literal_range ("01234\x35 789"
                                    ~^~~~~~~
   { dg-end-multiline-output "" } */
}

void
test_oct (void)
{
  /* Digits 0-9, expressing digit 5 in ASCII as "\065"
     and with a space in place of digit 6, to terminate the escaped
     octal code.  */
  __emit_string_literal_range ("01234\065 789", /* { dg-warning "range" } */
			       4, 3, 7);
/* { dg-begin-multiline-output "" }
   __emit_string_literal_range ("01234\065 789"
                                    ~^~~~~~~
   { dg-end-multiline-output "" } */
}

void
test_multiple (void)
{
  /* Digits 0-9, expressing digit 5 in ASCII as hex "\x35"
     digit 6 in ASCII as octal "\066", concatenating multiple strings.  */
  __emit_string_literal_range ("01234"  "\x35"  "\066"  "789", /* { dg-warning "range" } */
			       5, 3, 8);
/* { dg-begin-multiline-output "" }
   __emit_string_literal_range ("01234"  "\x35"  "\066"  "789",
                                    ~~~~~~^~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
}

void
test_ucn4 (void)
{
  /* Digits 0-9, expressing digits 5 and 6 as Roman numerals expressed
     as UCN 4.
     The resulting string is encoded as UTF-8.  Most of the digits are 1 byte
     each, but digits 5 and 6 are encoded with 3 bytes each.
     Hence to underline digits 4-7 we need to underling using bytes 4-11 in
     the UTF-8 encoding.  */
  __emit_string_literal_range ("01234\u2174\u2175789", /* { dg-warning "range" } */
			       5, 4, 11);
/* { dg-begin-multiline-output "" }
   __emit_string_literal_range ("01234\u2174\u2175789",
                                     ~^~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
}

void
test_ucn8 (void)
{
  /* Digits 0-9, expressing digits 5 and 6 as Roman numerals as UCN 8.
     The resulting string is the same as as in test_ucn4 above, and hence
     has the same UTF-8 encoding, and so we again need to underline bytes
     4-11 in the UTF-8 encoding in order to underline digits 4-7.  */
  __emit_string_literal_range ("01234\U00002174\U00002175789", /* { dg-warning "range" } */
			       5, 4, 11);
/* { dg-begin-multiline-output "" }
   __emit_string_literal_range ("01234\U00002174\U00002175789",
                                     ~^~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
}

void
test_u8 (void)
{
  /* Digits 0-9.  */
  __emit_string_literal_range (u8"0123456789", /* { dg-warning "range" } */
			       6, 4, 7);
/* { dg-begin-multiline-output "" }
   __emit_string_literal_range (u8"0123456789",
                                       ~~^~
   { dg-end-multiline-output "" } */
}

void
test_u (void)
{
  /* Digits 0-9.  */
  __emit_string_literal_range (u"0123456789", /* { dg-error "unable to read substring location: execution character set != source character set" } */
			       6, 4, 7);
/* { dg-begin-multiline-output "" }
   __emit_string_literal_range (u"0123456789",
                                ^~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
}

void
test_U (void)
{
  /* Digits 0-9.  */
  __emit_string_literal_range (U"0123456789", /* { dg-error "unable to read substring location: execution character set != source character set" } */
			       6, 4, 7);
/* { dg-begin-multiline-output "" }
   __emit_string_literal_range (U"0123456789",
                                ^~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
}

void
test_L (void)
{
  /* Digits 0-9.  */
  __emit_string_literal_range (L"0123456789", /* { dg-error "unable to read substring location: execution character set != source character set" } */
			       6, 4, 7);
/* { dg-begin-multiline-output "" }
   __emit_string_literal_range (L"0123456789",
                                ^~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
}

void
test_raw_string_one_liner (void)
{
  /* Digits 0-9.  */
  __emit_string_literal_range (R"foo(0123456789)foo", /* { dg-warning "range" } */
			       6, 4, 7);
/* { dg-begin-multiline-output "" }
   __emit_string_literal_range (R"foo(0123456789)foo",
                                          ~~^~
   { dg-end-multiline-output "" } */
}

void
test_raw_string_multiline (void)
{
  __emit_string_literal_range (R"foo(
hello
world
)foo",
			       6, 4, 7);
  /* { dg-error "unable to read substring location: range endpoints are on different lines" "" { target *-*-* } .-5 } */
  /* { dg-begin-multiline-output "" }
   __emit_string_literal_range (R"foo(
                                ^~~~~~
 hello
 ~~~~~                           
 world
 ~~~~~                           
 )foo",
 ~~~~~                           
   { dg-end-multiline-output "" } */
}

void
test_macro (void)
{
#define START "01234"  /* { dg-warning "range" } */
  __emit_string_literal_range (START
                               "56789",
                               4, 3, 6);
/* { dg-begin-multiline-output "" }
 #define START "01234"
                   ~^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   __emit_string_literal_range (START
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                "56789",
                                ~~~
   { dg-end-multiline-output "" } */
}

void
test_multitoken_macro (void)
{
#define RANGE ("0123456789")  /* { dg-error "unable to read substring location: macro expansion" } */
  __emit_string_literal_range (RANGE, 4, 3, 6);
/* { dg-begin-multiline-output "" }
 #define RANGE ("0123456789")
               ^~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
   __emit_string_literal_range (RANGE, 4, 3, 6);
                                ^~~~~
   { dg-end-multiline-output "" } */
#undef RANGE
}

/* Verify that the location of the closing quote is used
   for the location of the null terminating character.  */

void
test_terminator_location (void)
{
  __emit_string_literal_range ("0123456789", /* { dg-warning "range" } */
			       10, 10, 10);
/* { dg-begin-multiline-output "" }
   __emit_string_literal_range ("0123456789",
                                           ^
   { dg-end-multiline-output "" } */
}

/* Verify that we fail gracefully when a string literal token is split
   across multiple physical lines.  */

void
test_backslash_continued_logical_lines (void)
{
  __emit_string_literal_range ("\
01234\
56789", 6, 6, 7);
  /* { dg-error "unable to read substring location: range endpoints are on different lines" "" { target *-*-* } .-3 } */
  /* { dg-begin-multiline-output "" }
   __emit_string_literal_range ("\
                                ^~
 01234\
 ~~~~~~                          
 56789", 6, 6, 7);
 ~~~~~~                          
   { dg-end-multiline-output "" } */
}
