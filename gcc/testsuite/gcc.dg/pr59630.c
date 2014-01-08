/* { dg-do compile } */
/* { dg-options "-O" } */

_Bool foo()
{
  _Bool (*f)(int) = __builtin_abs; /* { dg-warning "" } */
  return f(0);
}
