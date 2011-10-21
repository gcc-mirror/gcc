/* { dg-do run } */
/* { dg-options "-Os -fomit-frame-pointer -mno-accumulate-outgoing-args -fno-asynchronous-unwind-tables" } */
/* { dg-options "-Os -fomit-frame-pointer -fno-asynchronous-unwind-tables" { target *-*-mingw* *-*-cygwin* } } */

extern void abort(void);

static void *p[2];

void __attribute__((noinline))
g(int x, ...)
{
  asm volatile ("" : : "g"(x));
}

void __attribute__((noinline))
f(int x)
{
  p[0] = __builtin_return_address (0);
  if (x == 0)
    g(0);
  g(1, 2, 3, 4, 5, 6, 7);

  asm goto ("jmp %l0" : : : : label);
  abort ();

 label:
  p[1] = __builtin_return_address (0);
}

int main()
{
  f(1);
  if (p[0] != p[1])
    abort ();
  return 0;
}
