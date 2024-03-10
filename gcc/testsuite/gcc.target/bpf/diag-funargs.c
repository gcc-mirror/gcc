/* Verify proper errors are generated for functions taking too many
   arguments.  */
/* { dg-do compile } */
/* { dg-options "-O0" } */

int
foo (int a1,  /* { dg-error "too many function arguments" } */
     int a2,
     int a3,
     int a4,
     int a5,
     int a6)
{
  return a1 + a2 + a3 + a4 + a5 + a6;
}

int
bar (int i1, int i2, int i3, int i4, int i5)
{
  return foo (i1, i2, i3, i4, i5, 10);
}
