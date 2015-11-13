/* { dg-do compile } */
/* { dg-options "-Wuninitialized -fdiagnostics-show-caret" } */

int test_uninit_1 (void)
{
  int result;
  return result;  /* { dg-warning "uninitialized" } */
/* { dg-begin-multiline-output "" }
   return result;
          ^~~~~~
   { dg-end-multiline-output "" } */
}

int test_uninit_2 (void)
{
  int result;
  result += 3; /* { dg-warning "uninitialized" } */
/* { dg-begin-multiline-output "" }
   result += 3;
   ~~~~~~~^~~~
   { dg-end-multiline-output "" } */
  return result;
}
