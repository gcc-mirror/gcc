/* { dg-do compile } */
/* { dg-options "-Wuninitialized -fdiagnostics-show-caret" } */

int test_uninit_1 (void)
{
  int result_1;     /* { dg-message "declared here" } */
  return result_1;  /* { dg-warning "uninitialized" } */
  /* { dg-begin-multiline-output "" }
   return result_1;
          ^~~~~~~~
   int result_1;
       ^~~~~~~~
   { dg-end-multiline-output "" } */
}

int test_uninit_2 (void)
{
  int result_2;     /* { dg-message "declared here" } */
  result_2 += 3;    /* { dg-warning "uninitialized" } */
  /* { dg-begin-multiline-output "" }
   result_2 += 3;
   ~~~~~~~~~^~~~
   int result_2;
       ^~~~~~~~
   { dg-end-multiline-output "" } */
  return result_2;
}
