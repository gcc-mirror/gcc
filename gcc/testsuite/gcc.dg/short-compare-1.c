/* PR rtl-optimization/19296 */
/* Origin: Falk Hueffner <falk@debian.org> */

/* { dg-do run } */
/* { dg-options "-O" } */
/* { dg-options "-O -mtune=i686" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */

extern void abort(void);

void f(unsigned short ad)
{
  if (ad >= 0x4000 && ad < 0xc000) 
    abort();
}

int main(void)
{
  f(0xff00); 
  return 0;
}
