/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -misa=sm_53 -mptx=_" } */
/* { dg-additional-options "-mexperimental" } */

_Float16 a;
_Float16 b;
_Float16 c;
_Float16 d;

void foo()
{
  a = (_Float16)(b*c) + d;
}

/* { dg-final { scan-assembler "fma.rn.f16" } } */
