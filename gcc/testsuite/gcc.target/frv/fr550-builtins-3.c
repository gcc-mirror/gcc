/* Test prefetch support.  */
/* { dg-do compile } */

#if __FRV_VLIW__ > 1
void foo (void *x)
{
  __data_prefetch (x);
}
#else
asm (";\tnop.p\n;\tnldub ");
#endif

/* { dg-final { scan-assembler "\tnop.p.*\tnldub " } } */
