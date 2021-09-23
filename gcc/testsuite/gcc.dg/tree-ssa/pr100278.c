/* { dg-do compile } */
/* { dg-options "-O2" } */

void a()
{
#if defined __s390__
  register int b asm("r5");
#elif defined __x86_64__
  register int b asm("eax");
#else
  volatile int b;
#endif
  if (b)
    b = 1;
  for (; b;)
    ;
}
