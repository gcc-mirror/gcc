/* { dg-do compile } */

/* See the notes in diagnostic-test-string-literals-1.c.
   This test case has caret-printing disabled.  */

extern void __emit_string_literal_range (const void *literal, int caret_idx,
					 int start_idx, int end_idx);
/* Test of a stringified macro argument, by itself.  */

void
test_stringified_token_1 (int x)
{
#define STRINGIFY(EXPR) #EXPR

  __emit_string_literal_range (STRINGIFY(x > 0), /* { dg-error "unable to read substring location: macro expansion" } */
                               0, 0, 4);

#undef STRINGIFY
}

/* Test of a stringified token within a concatenation.  */

void
test_stringized_token_2 (int x)
{
#define EXAMPLE(EXPR, CARET_IDX, START_IDX, END_IDX)		\
  do {								\
    __emit_string_literal_range ("  before " #EXPR " after \n",	\
				 CARET_IDX, START_IDX, END_IDX);	\
  } while (0)

  EXAMPLE(x > 0, 1, 1, 6);
  /* { dg-error "unable to read substring location: cpp_interpret_string_1 failed" "" { target *-*-* } 28 } */

#undef EXAMPLE
}

/* Test of a doubly-stringified macro argument (by itself).  */

void
test_stringified_token_3 (int x)
{
#define XSTR(s) STR(s)
#define STR(s) #s
#define FOO 123456789
  __emit_string_literal_range (XSTR (FOO), /* { dg-error "unable to read substring location: macro expansion" } */
                               2, 2, 3);

#undef XSTR
#undef STR
#undef FOO
}

