/* PR target/6753
   This testcase was miscompiled because sse_mov?fcc_const0*
   patterns were missing earlyclobber.  */
/* { dg-do run { target i386-*-* x86_64-*-* } } */
/* { dg-skip-if "" { ilp32 } { "-fpic" "-fPIC" } { "" } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-march=pentium3 -msse -ffast-math -O2" } */

extern void abort (void);
extern void exit (int);

float one = 1.f;

void bar (float f)
{
  if (__builtin_memcmp (&one, &f, sizeof (float)))
    abort ();
}

float foo (void)
{
  return 1.f;
}

typedef struct
{
  float t;
} T;

void bail_if_no_sse (void)
{
  int fl1, fl2;

  /* See if we can use cpuid.  */
  __asm__ ("pushfl; pushfl; popl %0; movl %0,%1; xorl %2,%0;"
	   "pushl %0; popfl; pushfl; popl %0; popfl"
	   : "=&r" (fl1), "=&r" (fl2)
	   : "i" (0x00200000));
  if (((fl1 ^ fl2) & 0x00200000) == 0)
    exit (0);

  /* See if cpuid gives capabilities.  */
  __asm__ ("cpuid" : "=a" (fl1) : "0" (0) : "ebx", "ecx", "edx", "cc");
  if (fl1 == 0)
    exit (0);

  /* See if capabilities include SSE (25th bit; 26 for SSE2).  */
  __asm__ ("cpuid" : "=a" (fl1), "=d" (fl2) : "0" (1) : "ebx", "ecx", "cc");
  if ((fl2 & (1 << 25)) == 0)
    exit (0);
}

int main (void)
{
  int i;
  T x[1];

  bail_if_no_sse ();
  for (i = 0; i < 1; i++)
    {
      x[i].t = foo ();
      x[i].t = 0.f > x[i].t ? 0.f : x[i].t;
      bar (x[i].t);
    }

  exit (0);
}
