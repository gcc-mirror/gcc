/* Due to a reload inheritance bug, the asm statement in f() would be passed
   the low part of u.ll on little-endian 32-bit targets.  */
/* { dg-do run { target mips*-*-* } } */

extern void abort (void);
extern void exit (int);

#if __mips_isa_rev <= 5
unsigned int g;

unsigned __attribute__ ((nomips16)) long long f (unsigned int x)
{
  union { unsigned long long ll; unsigned int parts[2]; } u;

  u.ll = ((unsigned long long) x * x);
  asm ("mflo\t%0" : "=r" (g) : "l" (u.parts[1]));
  return u.ll;
}
#endif

int __attribute__ ((nomips16)) main ()
{
#if __mips_isa_rev <= 5
  union { unsigned long long ll; unsigned int parts[2]; } u;

  u.ll = f (0x12345678);
  if (g != u.parts[1])
    abort ();
#endif
  exit (0);
}
