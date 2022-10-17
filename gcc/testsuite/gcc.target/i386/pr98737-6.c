/* PR target/98737 */
/* { dg-do compile } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-additional-options "-march=i686" { target ia32 } } */
/* { dg-final { scan-assembler "lock\[^\n\r]\*orq\t" { target lp64 } } } */
/* { dg-final { scan-assembler "lock\[^\n\r]\*orl\t" } } */
/* { dg-final { scan-assembler "lock\[^\n\r]\*orw\t" } } */
/* { dg-final { scan-assembler "lock\[^\n\r]\*orb\t" } } */
/* { dg-final { scan-assembler-not "lock\[^\n\r]\*xadd" } } */
/* { dg-final { scan-assembler-not "lock\[^\n\r]\*cmpxchg" } } */

long a;
int b;
short c;
char d;

int
f1 (long x)
{
  return __atomic_or_fetch (&a, x, __ATOMIC_RELEASE) == 0;
}

int
f2 (int x)
{
  return __atomic_or_fetch (&b, x, __ATOMIC_RELEASE) == 0;
}

int
f3 (short x)
{
  return __atomic_or_fetch (&c, x, __ATOMIC_RELEASE) == 0;
}

int
f4 (char x)
{
  return __atomic_or_fetch (&d, x, __ATOMIC_RELEASE) == 0;
}

int
f5 (long x)
{
  return __atomic_or_fetch (&a, x, __ATOMIC_RELEASE) != 0;
}

int
f6 (int x)
{
  return __atomic_or_fetch (&b, x, __ATOMIC_RELEASE) != 0;
}

int
f7 (short x)
{
  return __atomic_or_fetch (&c, x, __ATOMIC_RELEASE) != 0;
}

int
f8 (char x)
{
  return __atomic_or_fetch (&d, x, __ATOMIC_RELEASE) != 0;
}

int
f9 (long x)
{
  return __atomic_or_fetch (&a, x, __ATOMIC_RELEASE) < 0;
}

int
f10 (int x)
{
  return __atomic_or_fetch (&b, x, __ATOMIC_RELEASE) < 0;
}

int
f11 (short x)
{
  return __atomic_or_fetch (&c, x, __ATOMIC_RELEASE) < 0;
}

int
f12 (char x)
{
  return __atomic_or_fetch (&d, x, __ATOMIC_RELEASE) < 0;
}

int
f13 (long x)
{
  return __atomic_or_fetch (&a, x, __ATOMIC_RELEASE) >= 0;
}

int
f14 (int x)
{
  return __atomic_or_fetch (&b, x, __ATOMIC_RELEASE) >= 0;
}

int
f15 (short x)
{
  return __atomic_or_fetch (&c, x, __ATOMIC_RELEASE) >= 0;
}

int
f16 (char x)
{
  return __atomic_or_fetch (&d, x, __ATOMIC_RELEASE) >= 0;
}

int
f17 (long x)
{
  return __sync_or_and_fetch (&a, x) == 0;
}

int
f18 (int x)
{
  return __sync_or_and_fetch (&b, x) == 0;
}

int
f19 (short x)
{
  return __sync_or_and_fetch (&c, x) == 0;
}

int
f20 (char x)
{
  return __sync_or_and_fetch (&d, x) == 0;
}

int
f21 (long x)
{
  return __sync_or_and_fetch (&a, x) != 0;
}

int
f22 (int x)
{
  return __sync_or_and_fetch (&b, x) != 0;
}

int
f23 (short x)
{
  return __sync_or_and_fetch (&c, x) != 0;
}

int
f24 (char x)
{
  return __sync_or_and_fetch (&d, x) != 0;
}

int
f25 (long x)
{
  return __sync_or_and_fetch (&a, x) < 0;
}

int
f26 (int x)
{
  return __sync_or_and_fetch (&b, x) < 0;
}

int
f27 (short x)
{
  return __sync_or_and_fetch (&c, x) < 0;
}

int
f28 (char x)
{
  return __sync_or_and_fetch (&d, x) < 0;
}

int
f29 (long x)
{
  return __sync_or_and_fetch (&a, x) >= 0;
}

int
f30 (int x)
{
  return __sync_or_and_fetch (&b, x) >= 0;
}

int
f31 (short x)
{
  return __sync_or_and_fetch (&c, x) >= 0;
}

int
f32 (char x)
{
  return __sync_or_and_fetch (&d, x) >= 0;
}

int
f33 (long x)
{
  return __atomic_or_fetch (&a, x, __ATOMIC_RELEASE) <= 0;
}

int
f34 (int x)
{
  return __atomic_or_fetch (&b, x, __ATOMIC_RELEASE) <= 0;
}

int
f35 (short x)
{
  return __atomic_or_fetch (&c, x, __ATOMIC_RELEASE) <= 0;
}

int
f36 (char x)
{
  return __atomic_or_fetch (&d, x, __ATOMIC_RELEASE) <= 0;
}

int
f37 (long x)
{
  return __atomic_or_fetch (&a, x, __ATOMIC_RELEASE) > 0;
}

int
f38 (int x)
{
  return __atomic_or_fetch (&b, x, __ATOMIC_RELEASE) > 0;
}

int
f39 (short x)
{
  return __atomic_or_fetch (&c, x, __ATOMIC_RELEASE) > 0;
}

int
f40 (char x)
{
  return __atomic_or_fetch (&d, x, __ATOMIC_RELEASE) > 0;
}

int
f41 (long x)
{
  return __sync_or_and_fetch (&a, x) <= 0;
}

int
f42 (int x)
{
  return __sync_or_and_fetch (&b, x) <= 0;
}

int
f43 (short x)
{
  return __sync_or_and_fetch (&c, x) <= 0;
}

int
f44 (char x)
{
  return __sync_or_and_fetch (&d, x) <= 0;
}

int
f45 (long x)
{
  return __sync_or_and_fetch (&a, x) > 0;
}

int
f46 (int x)
{
  return __sync_or_and_fetch (&b, x) > 0;
}

int
f47 (short x)
{
  return __sync_or_and_fetch (&c, x) > 0;
}

int
f48 (char x)
{
  return __sync_or_and_fetch (&d, x) > 0;
}
