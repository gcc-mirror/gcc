/* __mips, and related defines, guarantee that certain assembly
   instructions can be used.  Check a few examples.  */
/* { dg-do run { target mips*-*-* } } */
typedef int int32 __attribute__ ((mode (SI)));
typedef int int64 __attribute__ ((mode (DI)));
int foo (float inf, int64 in64, int32 in32)
{
  int64 res64;
  int32 res32;

#if __mips != 1 && defined (__mips_hard_float)
  __asm__ ("trunc.w.s %0, %1" : "=f" (res32) : "f" (inf));
  if (res32 != 11)
    abort ();
#endif

#if defined (__mips64)
  __asm__ ("daddu %0, %1, %1" : "=r" (res64) : "r" (in64));
  if (res64 != 50)
    abort ();
#endif

#if (__mips == 4 || __mips == 32 || __mips == 64) && !defined (__mips16)
  __asm__ ("move %0,%.\n\tmovn %0,%1,%2"
	   : "=&r" (res32) : "r" (in32), "r" (in64 != 0));
  if (res32 != 60)
    abort ();
#endif
}

int main ()
{
  foo (11.4f, 25, 60);
  exit (0);
}
