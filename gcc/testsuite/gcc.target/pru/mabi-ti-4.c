/* Test TI ABI with supported constructs */

/* { dg-do assemble } */
/* { dg-options "-O1 -mabi=ti" } */


extern void extfunc1(long long);
extern long long extfunc2(long long);

long long test(void)
{
  extfunc1(3);
  return extfunc2(1);
}
