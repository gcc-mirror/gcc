/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-mb -O2 -fomit-frame-pointer" } */
/* { dg-final { scan-assembler "mov @r.,r.; mov @\\(4,r.\\),r." } } */
double d;

double
f (void)
{
  double r;

/* If -ml from the target options is passed after -mb from dg-options, we
   end up with th reverse endianness.  */
#if TARGET_SHMEDIA || defined (__LITTLE_ENDIAN__)
  asm ("mov @r1,r3; mov @(4,r1),r4");
#else
  asm ("mov %S1,%S0; mov %R1,%R0" : "=&r" (r) : "m" (d));
#endif
  return r;
}
