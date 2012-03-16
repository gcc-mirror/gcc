/* PR middle-end/52592 */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -O2 -ffast-math" } */

#define T(type, name) \
type name (type);		\
__attribute__((cold))		\
int f##name (type x)		\
{				\
  return (int) name (x);	\
}

T (double, round)
T (float, roundf)
T (long double, roundl)
T (double, rint)
T (float, rintf)
T (long double, rintl)

/* { dg-final { scan-assembler-not "__builtin_iround" } } */
/* { dg-final { scan-assembler-not "__builtin_irint" } } */
