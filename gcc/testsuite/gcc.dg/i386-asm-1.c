/* PR inline-asm/11676 */
/* { dg-do run { target i?86-*-* } } */
/* { dg-skip-if "" { i?86-*-* } { "-m64" } { "" } } */
/* { dg-options "-O2" } */

extern void abort (void);
static int bar(int x) __asm__("bar") __attribute__((regparm(1)));
static int __attribute__((regparm(1), noinline, used))
bar(int x)
{
  if (x != 0)
    abort ();
}

static int __attribute__((regparm(1), noinline))
foo(int x)
{
  x = 0;
  __asm__ __volatile__("call bar" : "=a"(x) : "a"(x));
}

int main()
{
  foo(1);
  return 0;
}
