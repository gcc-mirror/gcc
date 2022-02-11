/* { dg-do compile } */
/* { dg-options "-O2 -misa=sm_53 -mptx=6.3 -ffast-math" } */

_Float16 a;
_Float16 b;
_Float16 c;
_Float16 d;

void foo()
{
  a = (_Float16)(b*c) + d;
}

/* { dg-final { scan-assembler "fma.rn.f16" } } */
