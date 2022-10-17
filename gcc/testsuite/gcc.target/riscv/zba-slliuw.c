/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zba_zbs -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" } } */

long
foo (long i)
{
  return (long)(unsigned int)i << 10;
}
/* XXX: This pattern need combine improvement or intermediate instruction
 *      from zbs.   */
/* { dg-final { scan-assembler "slli.uw" } } */
