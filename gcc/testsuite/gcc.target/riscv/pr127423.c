/* PR rtl-optimization/127423 */
/* { dg-do compile } */
/* { dg-options "-O2 -g -fno-tree-forwprop -fdisable-tree-phiopt1 -fdisable-tree-phiopt2 -fdisable-tree-phiopt3 -fdisable-tree-phiopt4 -march=rv64gc_zbb -mabi=lp64d -ffunction-sections -fcompare-debug" } */
/* { dg-final { scan-assembler-times {\mmin\t} 1 } } */

#define ATTR __attribute__ ((noinline, noclone))

ATTR int
debug_payload_min (int a, int b)
{
  _Bool pred = a < b;
  asm volatile ("" : : "r" (pred));
  int result = b;
  int xghost = a;
  if (pred)
    result = a;
  return result;
}
