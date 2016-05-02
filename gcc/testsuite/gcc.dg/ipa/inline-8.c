/* Verify that we do not inline isnanf test info -ffast-math code but that we
   do inline trivial functions across -Ofast boundary.  */
/* { dg-do run } */
/* { dg-options "-O2"  } */
#include <math.h>
/* Can't be inlined because isnanf will be optimized out.  */
int
cmp (float a)
{
  return isnanf (a);
}
/* Can be inlined.  */
int
move (int a)
{
  return a;
}
float a;
void
set ()
{
 a=nan("");
}
float b;
__attribute__ ((optimize("Ofast")))
int
main()
{
  b++;
  if (cmp(a))
    __builtin_abort ();
  float a = move (1);
  if (!__builtin_constant_p (a))
    __builtin_abort ();
  return 0;
}
