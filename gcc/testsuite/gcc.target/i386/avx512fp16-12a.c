/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16" } */

_Float16
__attribute__ ((noinline, noclone))
do_max (_Float16 __A, _Float16 __B)
{
  return __A > __B ? __A : __B;
}

_Float16
__attribute__ ((noinline, noclone))
do_min (_Float16 __A, _Float16 __B)
{
  return __A < __B ? __A : __B;
}

/* { dg-final { scan-assembler-times "vmaxsh\[ \\t\]" 1 } } */
/* { dg-final { scan-assembler-times "vminsh\[ \\t\]" 1 } } */
/* { dg-final { scan-assembler-not "vmovsh\[ \\t\]" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "vcomish\[ \\t\]" } } */
