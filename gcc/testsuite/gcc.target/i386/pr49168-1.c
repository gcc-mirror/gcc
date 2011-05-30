/* PR target/49168  */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mtune=generic" } */
/* { dg-final { scan-assembler-not "movdqa\[\t \]*%xmm\[0-9\]\+,\[^,\]*" } } */
/* { dg-final { scan-assembler "movdqu\[\t \]*%xmm\[0-9\]\+,\[^,\]*" } } */

void
flt128_va (void *mem, __float128 d)
{ 
  __builtin_memcpy (mem, &d, sizeof (d));
}
