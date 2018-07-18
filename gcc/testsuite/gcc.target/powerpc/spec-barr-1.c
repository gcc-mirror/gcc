/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-mcpu=power7" } */

void foo ()
{
  __builtin_ppc_speculation_barrier ();
}

/* { dg-final { scan-assembler "ori 31,31,0" } } */
