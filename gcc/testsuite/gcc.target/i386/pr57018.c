/* { dg-do run } */
/* { dg-options "-Os -fomit-frame-pointer -fno-asynchronous-unwind-tables" } */
/* { dg-additional-options "-march=i686" { target ia32 } } */

struct A { char a[16]; } a;

void __attribute__((noinline, noclone))
foo (struct A b)
{
  if (__builtin_memcmp (b.a, "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0", 16))
    __builtin_abort ();
  asm volatile ("" : : : "memory");
}

void __attribute__((noinline, noclone))
bar (struct A b)
{
  foo (a);
  a = b;
}

int
main ()
{
  struct A b = { "\0\1\2\3\4\5\6\7\10\11\12\13\14\15\16\17" };
  bar (b);
  if (__builtin_memcmp (a.a, b.a, 16))
    __builtin_abort ();
  return 0;
}

