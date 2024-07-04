/* { dg-do compile } */
/* { dg-options "-O -fno-tree-ch --param=max-completely-peel-times=0 -march=rv64iv -mabi=lp64d -fcompare-debug" } */


void
foo(void) {
  for (unsigned i = 0; i < sizeof(foo); i++)
    __builtin_printf("%d", i);
}
