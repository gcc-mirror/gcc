/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power7" } */

void foo ()
{
  __builtin_ppc_speculation_barrier ();
}

/* { dg-final { scan-assembler {ori\s+r?31,r?31,r?0} } } */
