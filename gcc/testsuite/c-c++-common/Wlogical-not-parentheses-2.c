/* { dg-do compile } */
/* { dg-options "-Wlogical-not-parentheses -fdiagnostics-show-caret" } */

 /* Test fixit hints.  */

int
foo (int aaa, int bbb)
{
  int r = 0;
  r += (!aaa) == bbb;
  r += !aaa == bbb; /* { dg-warning "logical not is only applied" } */
/* { dg-begin-multiline-output "" }
   r += !aaa == bbb;
             ^~
   r += !aaa == bbb;
        ^~~~
        (   )
   { dg-end-multiline-output "" } */
  return r;
}
