/* { dg-do compile { target float16 } } */
/* { dg-require-effective-target s390_mvx } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** test_asm_constant_zero_via_f:
**     vzero	%v([0-9]+)
**     foo	%f\1
**     br	%r14
*/

void
test_asm_constant_zero_via_f (void)
{
  __asm__ __volatile__ ("foo\t%0" :: "f" (0.f16));
}

/*
** test_asm_constant_zero_via_v:
**     vzero	%v([0-9]+)
**     foo	%f\1
**     br	%r14
*/

void
test_asm_constant_zero_via_v (void)
{
  __asm__ __volatile__ ("foo\t%0" :: "v" (0.f16));
}

/*
** test_asm_constant_via_f:
**     larl	(%r[0-9]+),\.L[0-9]+
**     vleh	%v([0-9]+),\.L[0-9]+-\.L[0-9]+\(\1\),0
**     foo	%f\2
**     br	%r14
*/

void
test_asm_constant_via_f (void)
{
  __asm__ __volatile__ ("foo\t%0" :: "f" (42.f16));
}

/*
** test_asm_constant_via_v:
**     larl	(%r[0-9]+),\.L[0-9]+
**     vleh	%v([0-9]+),\.L[0-9]+-\.L[0-9]+\(\1\),0
**     foo	%f\2
**     br	%r14
*/

void
test_asm_constant_via_v (void)
{
  __asm__ __volatile__ ("foo\t%0" :: "v" (42.f16));
}

/*
** test_asm_in_float16_via_f:
**     foo	%f0
**     br	%r14
*/

void
test_asm_in_float16_via_f (_Float16 x)
{
  __asm__ __volatile__ ("foo\t%0" :: "f" (x));
}

/*
** test_asm_in_float16_via_v:
**     foo	%f0
**     br	%r14
*/

void
test_asm_in_float16_via_v (_Float16 x)
{
  __asm__ __volatile__ ("foo\t%0" :: "v" (x));
}

/*
** test_asm_in_float16_via_r:
**     vlgvh	(%r[0-9]+),%v0,0
**     foo	\1
**     br	%r14
*/

void
test_asm_in_float16_via_r (_Float16 x)
{
  __asm__ __volatile__ ("foo\t%0" :: "r" (x));
}

/*
** test_asm_in_ushort_via_f:
**     vlvgh	%v([0-9]+),%r2,0
**     foo	%f\1
**     br	%r14
*/

void
test_asm_in_ushort_via_f (unsigned short x)
{
  __asm__ __volatile__ ("foo\t%0" :: "f" (x));
}

/*
** test_asm_in_ushort_via_v:
**     vlvgh	%v([0-9]+),%r2,0
**     foo	%f\1
**     br	%r14
*/

void
test_asm_in_ushort_via_v (unsigned short x)
{
  __asm__ __volatile__ ("foo\t%0" :: "v" (x));
}

/*
** test_asm_out_float16_via_f:
**     foo	%f0
**     br	%r14
*/

_Float16
test_asm_out_float16_via_f (void)
{
  _Float16 x;
  __asm__ ("foo\t%0" : "=f" (x));
  return x;
}

/*
** test_asm_out_float16_via_v:
**     foo	%f0
**     br	%r14
*/

_Float16
test_asm_out_float16_via_v (void)
{
  _Float16 x;
  __asm__ ("foo\t%0" : "=v" (x));
  return x;
}

/*
** test_asm_out_float16_via_r:
**     foo	(%r[0-9]+)
**     vlvgh	%v0,\1,0
**     br	%r14
*/

_Float16
test_asm_out_float16_via_r (void)
{
  _Float16 x;
  __asm__ ("foo\t%0" : "=r" (x));
  return x;
}

/*
** test_asm_out_ushort_via_f:
**     foo	%f([0-9]+)
**     vlgvh	%r2,%v\1,0
**     br	%r14
*/

unsigned short
test_asm_out_ushort_via_f (void)
{
  unsigned short x;
  __asm__ ("foo\t%0" : "=f" (x));
  return x;
}

/*
** test_asm_out_ushort_via_v:
**     foo	%f([0-9]+)
**     vlgvh	%r2,%v\1,0
**     br	%r14
*/

unsigned short
test_asm_out_ushort_via_v (void)
{
  unsigned short x;
  __asm__ ("foo\t%0" : "=v" (x));
  return x;
}
