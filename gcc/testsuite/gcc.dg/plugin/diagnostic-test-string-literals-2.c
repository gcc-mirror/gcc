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

  __emit_string_literal_range (STRINGIFY(x > 0), /* { dg-error "macro expansion|cpp_interpret_string_1 failed" } */
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
  /* { dg-error "unable to read substring location: cpp_interpret_string_1 failed" "" { target *-*-* } .-5 } */

#undef EXAMPLE
}

/* Test of a doubly-stringified macro argument (by itself).  */

void
test_stringified_token_3 (int x)
{
#define XSTR(s) STR(s)
#define STR(s) #s
#define FOO 123456789
  __emit_string_literal_range (XSTR (FOO), /* { dg-error "macro expansion|cpp_interpret_string_1 failed" } */
                               2, 2, 3);

#undef XSTR
#undef STR
#undef FOO
}

/* Test of a stringified macro argument within a concatenation.  */

void
test_pr79210 (void)
{
#define lpfc_vport_param_init(attr)    \
       __emit_string_literal_range ( \
                  "0423 lpfc_"#attr" attribute cannot be set to %d, "\
                  "allowed range is [0, 1]\n", 54, 53, 54) \

#define LPFC_VPORT_ATTR_R(name, decc)		\
  unsigned int lpfc_##name;			\
  lpfc_vport_param_init(name) \

  LPFC_VPORT_ATTR_R(peer_port_login,
  "some multiline blurb with a short final line "
  "here");

  /* { dg-error "19: unable to read substring location: range endpoints are on different lines" "" { target c } .-11 } */
  /* { dg-error "19: unable to read substring location: line is not wide enough" "" { target c++ } .-12 } */

#undef LPFC_VPORT_ATTR_R
#undef lpfc_vport_param_init
}
