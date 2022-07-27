/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -misa=sm_53 -mptx=_" } */
/* { dg-additional-options "-mexperimental" } */

_Float16 var;

void foo()
{
  var = (var < (_Float16)0.0) ? -var : var;
}

/* { dg-final { scan-assembler "and.b16" } } */
