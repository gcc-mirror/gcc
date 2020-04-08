/* { dg-do compile }  */

/* PR middle-end/94120  */

void foo()
{
  int foo;
  {
    #pragma acc declare copy(foo)  /* { dg-error "'foo' must be a variable declared in the same scope as '#pragma acc declare'" }  */
  }
}

void
f_data (void)
{
  int B[10];
#pragma acc data
  {
# pragma acc declare copy(B)  /* { dg-error "'B' must be a variable declared in the same scope as '#pragma acc declare'" }  */
    for (int i = 0; i < 10; i++)
      B[i] = -i;
  }
}
