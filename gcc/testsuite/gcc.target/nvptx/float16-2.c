/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -misa=sm_80 -mptx=_" } */
/* { dg-additional-options "-mexperimental" } */

_Float16 x;
_Float16 y;
_Float16 t;

void foo()
{
  t = x < y ? x : y;
}

void bar()
{
  t = x > y ? x : y;
}

/* { dg-final { scan-assembler "min.f16" } } */
/* { dg-final { scan-assembler "max.f16" } } */

