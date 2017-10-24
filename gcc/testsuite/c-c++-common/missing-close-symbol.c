/* { dg-options "-fdiagnostics-show-caret" } */

/* Verify that the C/C++ frontends show the pertinent opening symbol when
   a closing symbol is missing.  */

/* Verify that, when they are on the same line, that the opening symbol is
   shown as a secondary range within the main diagnostic.  */

void test_static_assert_same_line (void)
{
  _Static_assert(sizeof(int) >= sizeof(char), "msg"; /* { dg-error "expected '\\)' before ';' token" } */
  /* { dg-begin-multiline-output "" }
   _Static_assert(sizeof(int) >= sizeof(char), "msg";
                 ~                                  ^
                                                    )
     { dg-end-multiline-output "" } */
}

/* Verify that, when they are on different lines, that the opening symbol is
   shown via a secondary diagnostic.  */

void test_static_assert_different_line (void)
{
  _Static_assert(sizeof(int) >= sizeof(char), /* { dg-message "to match this '\\('" } */
		 "msg"; /* { dg-error "expected '\\)' before ';' token" } */
  /* { dg-begin-multiline-output "" }
    "msg";
         ^
         )
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
   _Static_assert(sizeof(int) >= sizeof(char),
                 ^
     { dg-end-multiline-output "" } */
}
