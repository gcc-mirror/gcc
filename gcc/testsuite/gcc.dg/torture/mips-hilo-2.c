/* Due to a reload inheritance bug, the asm statement in f() would be passed
   the low part of u.ll on little-endian 32-bit targets.  */
/* { dg-do run { target mips*-*-* } } */

extern void abort (void);
extern void exit (int);

#if !defined(__mips16)
unsigned int g;

unsigned long long f (unsigned int x)
{
  union { unsigned long long ll; unsigned int parts[2]; } u;

  u.ll = ((unsigned long long) x * x);
  asm ("mflo\t%0" : "=r" (g) : "l" (u.parts[1]));
  return u.ll;
}

int main ()
{
  union { unsigned long long ll; unsigned int parts[2]; } u;

  u.ll = f (0x12345678);
  if (g != u.parts[1])
    abort ();
  exit (0);
}
#else
int main () { exit (0); }
#endif
