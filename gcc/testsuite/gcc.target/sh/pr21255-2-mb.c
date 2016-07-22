/* { dg-do compile { target { big_endian } } }  */
/* { dg-options "-O2 -fomit-frame-pointer" } */
/* { dg-final { scan-assembler "mov @r.,r.; mov @\\(4,r.\\),r." } } */
double d;

double
f (void)
{
  double r;

/* If -ml from the target options is passed after -mb from dg-options, we
   end up with th reverse endianness.  */
  asm ("mov %S1,%S0; mov %R1,%R0" : "=&r" (r) : "m" (d));
  return r;
}
