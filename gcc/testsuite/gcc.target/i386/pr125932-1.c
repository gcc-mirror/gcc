/* PR target/125932 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -fdump-rtl-stv2" } */

typedef int v4si __attribute__ ((vector_size (16)));

int a;

void b (v4si c)
{
  __asm__ goto ("" : "=r" (a) : : : d);

 d:
  a &= c[1] & c[2] & c[3];
}

/* { dg-final { scan-rtl-dump "aborting chain" "stv2" } } */
