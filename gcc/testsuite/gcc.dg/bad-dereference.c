/* { dg-options "-fdiagnostics-show-caret" } */

struct foo
{
  int x;
};

int test_1 (struct foo some_f)
{
  return *some_f.x; /* { dg-error "invalid type argument of unary ... .have .int.." } */
/* { dg-begin-multiline-output "" }
   return *some_f.x;
          ^~~~~~~~~
   { dg-end-multiline-output "" } */
}

int test_2 (struct foo some_f)
{
  return *some_f; /* { dg-error "invalid type argument of unary ... .have .struct foo.." } */
/* { dg-begin-multiline-output "" }
   return *some_f;
          ^~~~~~~
   { dg-end-multiline-output "" } */
}
