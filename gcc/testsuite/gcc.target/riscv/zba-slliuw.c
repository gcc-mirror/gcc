/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zba -mabi=lp64 -O2" } */

long
foo (long i)
{
  return (long)(unsigned int)i << 10;
}
/* XXX: This pattern need combine improvement or intermediate instruction
 *      from zbs.   */
/* { dg-final { scan-assembler-not "slli.uw" } } */
