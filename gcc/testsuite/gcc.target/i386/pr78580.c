/* PR rtl-optimization/78580 */
/* { dg-do compile } */
/* { dg-options "-O0 -ffixed-ebx" } */

extern const signed char a;

int
foo (signed char x)
{
  return x;
}

int
main ()
{
  foo (a);
  return 0;
}
