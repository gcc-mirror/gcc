/* PR target/98737 */
/* { dg-do compile } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-additional-options "-march=i686" { target ia32 } } */
/* { dg-final { scan-assembler-not "lock\[^\n\r]\*subq\t" } } */
/* { dg-final { scan-assembler-not "lock\[^\n\r]\*subl\t" } } */
/* { dg-final { scan-assembler-not "lock\[^\n\r]\*subw\t" } } */
/* { dg-final { scan-assembler-not "lock\[^\n\r]\*subb\t" } } */
/* { dg-final { scan-assembler "lock\[^\n\r]\*xadd" } } */
/* { dg-final { scan-assembler-not "lock\[^\n\r]\*cmpxchg" } } */

long a;
int b;
short c;
char d;

int
f1 (long x)
{
  return __atomic_sub_fetch (&a, x, __ATOMIC_RELEASE) <= 0;
}

int
f2 (int x)
{
  return __atomic_sub_fetch (&b, x, __ATOMIC_RELEASE) <= 0;
}

int
f3 (short x)
{
  return __atomic_sub_fetch (&c, x, __ATOMIC_RELEASE) <= 0;
}

int
f4 (char x)
{
  return __atomic_sub_fetch (&d, x, __ATOMIC_RELEASE) <= 0;
}

int
f5 (long x)
{
  return __atomic_sub_fetch (&a, x, __ATOMIC_RELEASE) > 0;
}

int
f6 (int x)
{
  return __atomic_sub_fetch (&b, x, __ATOMIC_RELEASE) > 0;
}

int
f7 (short x)
{
  return __atomic_sub_fetch (&c, x, __ATOMIC_RELEASE) > 0;
}

int
f8 (char x)
{
  return __atomic_sub_fetch (&d, x, __ATOMIC_RELEASE) > 0;
}

int
f9 (long x)
{
  return __sync_sub_and_fetch (&a, x) <= 0;
}

int
f10 (int x)
{
  return __sync_sub_and_fetch (&b, x) <= 0;
}

int
f11 (short x)
{
  return __sync_sub_and_fetch (&c, x) <= 0;
}

int
f12 (char x)
{
  return __sync_sub_and_fetch (&d, x) <= 0;
}

int
f13 (long x)
{
  return __sync_sub_and_fetch (&a, x) > 0;
}

int
f14 (int x)
{
  return __sync_sub_and_fetch (&b, x) > 0;
}

int
f15 (short x)
{
  return __sync_sub_and_fetch (&c, x) > 0;
}

int
f16 (char x)
{
  return __sync_sub_and_fetch (&d, x) > 0;
}
