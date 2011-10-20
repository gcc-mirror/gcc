/* { dg-do run } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-O2 -Wno-psabi -fomit-frame-pointer -fno-asynchronous-unwind-tables -mpush-args -mno-accumulate-outgoing-args -m128bit-long-double" } */
/* { dg-options "-O2 -Wno-psabi -fomit-frame-pointer -fno-asynchronous-unwind-tables -mpush-args -m128bit-long-double" { target *-*-mingw* *-*-cygwin* } } */

void abort (void);

void __attribute__((noinline))
f (long double a)
{
  if (a != 1.23L)
    abort ();
}

int __attribute__((noinline))
g (long double b)
{
  f (b);
  return 0;
}

int
main (void)
{
  g (1.23L);
  return 0;
}
