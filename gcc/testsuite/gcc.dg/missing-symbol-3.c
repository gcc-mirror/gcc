/* { dg-options "-fdiagnostics-show-caret" } */

/* A sequence of bogus _Static_assert.
   We can offer fix-it hints for some of these, but not all.  */

void test_static_assert_1 (void)
{
  _Static_assert sizeof(int) >= sizeof(char); /* { dg-error "expected '\\(' before 'sizeof'" } */
  /* { dg-begin-multiline-output "" }
   _Static_assert sizeof(int) >= sizeof(char);
                  ^~~~~~
                  (
     { dg-end-multiline-output "" } */
}

void test_static_assert_2 (void)
{
  _Static_assert(sizeof(int) >= sizeof(char); /* { dg-error "expected ',' before ';' token" } */
  /* { dg-begin-multiline-output "" }
   _Static_assert(sizeof(int) >= sizeof(char);
                                             ^
                                             ,
     { dg-end-multiline-output "" } */
}

void test_static_assert_3 (void)
{
  _Static_assert(sizeof(int) >= sizeof(char),; /* { dg-error "expected string literal before ';' token" } */
  /* { dg-begin-multiline-output "" }
   _Static_assert(sizeof(int) >= sizeof(char),;
                                              ^
     { dg-end-multiline-output "" } */
}

void test_static_assert_4 (void)
{
  _Static_assert(sizeof(int) >= sizeof(char), "msg"; /* { dg-error "expected '\\)' before ';' token" } */
  /* { dg-begin-multiline-output "" }
   _Static_assert(sizeof(int) >= sizeof(char), "msg";
                 ~                                  ^
                                                    )
     { dg-end-multiline-output "" } */
}

/* The final one is correct.  */

void test_static_assert_5 (void)
{
  _Static_assert(sizeof(int) >= sizeof(char), "msg");
}
