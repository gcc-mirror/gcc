/* { dg-do compile } */
/* { dg-options "-O2" } */

extern char foo;
extern unsigned char bar;

int
foo1 (void)
{
  return __sync_fetch_and_and (&foo, ~1) & 1;
}

int
foo2 (void)
{
  return __sync_fetch_and_or (&foo, 1) & 1;
}

int
foo3 (void)
{
  return __sync_fetch_and_xor (&foo, 1) & 1;
}

unsigned short
bar1 (void)
{
  return __sync_fetch_and_and (&bar, ~1) & 1;
}

unsigned short
bar2 (void)
{
  return __sync_fetch_and_or (&bar, 1) & 1;
}

unsigned short
bar3 (void)
{
  return __sync_fetch_and_xor (&bar, 1) & 1;
}

/* { dg-final { scan-assembler-times "lock;?\[ \t\]*cmpxchgb" 6 { target { x86_64-*-* i?86-*-* } } } } */
