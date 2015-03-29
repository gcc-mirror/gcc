/* { dg-do link } */
/* { dg-require-effective-target lto } */
/* { dg-options "-O2 -flto" } */
#ifdef __x86_64__
register volatile int a __asm__("%rsp");
#else
register volatile int a __asm__("%esp");
#endif
__attribute__ ((used))
int t () { a = 0; }
int main () { a = 0; }
