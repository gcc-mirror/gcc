/* PR target/98737 */
/* { dg-do compile } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-additional-options "-march=i686" { target ia32 } } */
/* { dg-final { scan-assembler "lock\[^\n\r]\*addq\t" { target lp64 } } } */
/* { dg-final { scan-assembler "lock\[^\n\r]\*addl\t" } } */
/* { dg-final { scan-assembler "lock\[^\n\r]\*addw\t" } } */
/* { dg-final { scan-assembler "lock\[^\n\r]\*addb\t" } } */
/* { dg-final { scan-assembler-not "lock\[^\n\r]\*xadd" } } */
/* { dg-final { scan-assembler-not "lock\[^\n\r]\*cmpxchg" } } */

long a;
int b;
short c;
char d;

int
f1 (long x)
{
  return __atomic_add_fetch (&a, x, __ATOMIC_RELEASE) == 0;
}

int
f2 (int x)
{
  return __atomic_add_fetch (&b, x, __ATOMIC_RELEASE) == 0;
}

int
f3 (short x)
{
  return __atomic_add_fetch (&c, x, __ATOMIC_RELEASE) == 0;
}

int
f4 (char x)
{
  return __atomic_add_fetch (&d, x, __ATOMIC_RELEASE) == 0;
}

int
f5 (long x)
{
  return __atomic_add_fetch (&a, x, __ATOMIC_RELEASE) != 0;
}

int
f6 (int x)
{
  return __atomic_add_fetch (&b, x, __ATOMIC_RELEASE) != 0;
}

int
f7 (short x)
{
  return __atomic_add_fetch (&c, x, __ATOMIC_RELEASE) != 0;
}

int
f8 (char x)
{
  return __atomic_add_fetch (&d, x, __ATOMIC_RELEASE) != 0;
}

int
f9 (long x)
{
  return __atomic_add_fetch (&a, x, __ATOMIC_RELEASE) < 0;
}

int
f10 (int x)
{
  return __atomic_add_fetch (&b, x, __ATOMIC_RELEASE) < 0;
}

int
f11 (short x)
{
  return __atomic_add_fetch (&c, x, __ATOMIC_RELEASE) < 0;
}

int
f12 (char x)
{
  return __atomic_add_fetch (&d, x, __ATOMIC_RELEASE) < 0;
}

int
f13 (long x)
{
  return __atomic_add_fetch (&a, x, __ATOMIC_RELEASE) >= 0;
}

int
f14 (int x)
{
  return __atomic_add_fetch (&b, x, __ATOMIC_RELEASE) >= 0;
}

int
f15 (short x)
{
  return __atomic_add_fetch (&c, x, __ATOMIC_RELEASE) >= 0;
}

int
f16 (char x)
{
  return __atomic_add_fetch (&d, x, __ATOMIC_RELEASE) >= 0;
}

int
f17 (long x)
{
  return __sync_add_and_fetch (&a, x) == 0;
}

int
f18 (int x)
{
  return __sync_add_and_fetch (&b, x) == 0;
}

int
f19 (short x)
{
  return __sync_add_and_fetch (&c, x) == 0;
}

int
f20 (char x)
{
  return __sync_add_and_fetch (&d, x) == 0;
}

int
f21 (long x)
{
  return __sync_add_and_fetch (&a, x) != 0;
}

int
f22 (int x)
{
  return __sync_add_and_fetch (&b, x) != 0;
}

int
f23 (short x)
{
  return __sync_add_and_fetch (&c, x) != 0;
}

int
f24 (char x)
{
  return __sync_add_and_fetch (&d, x) != 0;
}

int
f25 (long x)
{
  return __sync_add_and_fetch (&a, x) < 0;
}

int
f26 (int x)
{
  return __sync_add_and_fetch (&b, x) < 0;
}

int
f27 (short x)
{
  return __sync_add_and_fetch (&c, x) < 0;
}

int
f28 (char x)
{
  return __sync_add_and_fetch (&d, x) < 0;
}

int
f29 (long x)
{
  return __sync_add_and_fetch (&a, x) >= 0;
}

int
f30 (int x)
{
  return __sync_add_and_fetch (&b, x) >= 0;
}

int
f31 (short x)
{
  return __sync_add_and_fetch (&c, x) >= 0;
}

int
f32 (char x)
{
  return __sync_add_and_fetch (&d, x) >= 0;
}
