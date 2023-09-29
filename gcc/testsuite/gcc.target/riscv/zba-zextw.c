/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zba_zbs -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

long
foo (long i)
{
  return (long)(unsigned int)i;
}
/* XXX: This pattern require combine improvement.   */
/* { dg-final { scan-assembler-not {\mslli\.uw\M} } } */
