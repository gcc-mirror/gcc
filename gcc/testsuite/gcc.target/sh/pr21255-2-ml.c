/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-ml -O2 -fomit-frame-pointer" } */
/* { dg-final { scan-assembler "mov @\\(4,r.\\),r.; mov @r.,r." } } */
double d;

double
f (void)
{
  double r;

/* If -mb from the target options is passed after -ml from dg-options, we
   end up with th reverse endianness.  */
#if TARGET_SHMEDIA || defined (__BIG_ENDIAN__)
  asm ("mov @(4,r1),r4; mov @r1,r3");
#else
  asm ("mov %S1,%S0; mov %R1,%R0" : "=&r" (r) : "m" (d));
#endif
  return r;
}
