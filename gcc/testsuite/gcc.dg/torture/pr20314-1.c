/* PR inline-asm/20314 */
/* { dg-do compile { target i?86-*-* x86_64-*-* powerpc*-*-* ia64-*-* } } */

int
f1 (void)
{
  int x = 4, y;
  __asm__ volatile ("" : "+r,r" (x), "=r,r" (y)
		       : "%r,r" (x), "m,r" (8), "r,r" (2));
  return x;
}

int
f2 (void)
{
  int x = 4, y;
  __asm__ volatile ("" : "=r,r" (x), "=r,r" (y)
		       : "%0,0" (x), "m,r" (8), "r,r" (2));
  return x;
}

int
f3 (void)
{
  int x = 4, y;
  __asm__ volatile ("" : "+r,r" (x), "=r,r" (y)
		       : "%m,r" (8), "r,r" (2));
  return x;
}

int
f4 (void)
{
  int x = 4, y;
  __asm__ volatile ("" : "+r" (x), "=r" (y)
		       : "r" (x), "r" (8), "r" (2));
  return x;
}

int
f5 (void)
{
  int x = 4, y;
  __asm__ volatile ("" : "=r" (x), "=r" (y)
		       : "0" (x), "r" (8), "r" (2));
  return x;
}

int
f6 (void)
{
  int x = 4, y;
  __asm__ volatile ("" : "+r" (x), "=r" (y)
		       : "r" (8), "r" (2));
  return x;
}
