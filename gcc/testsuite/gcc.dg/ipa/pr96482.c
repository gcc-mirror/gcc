/* PR ipa/96482 */
/* { dg-do run } */
/* { dg-options "-O2 -flto"  } */
/* { dg-require-effective-target lto } */

int
__attribute__((noinline))
foo(int arg)
{
  if (arg == 3)
    return 1;
  if (arg == 4)
    return 123;

  __builtin_unreachable ();
}

int
__attribute__((noinline))
baz(int x)
{
  if (x != 0)
    return foo(3); /* called */

  return 1;
}

int
__attribute__((noinline))
bar(int x)
{
  if (x == 0)
    return foo(5); /* not executed */

  return 1;
}

int main(int argc, char **argv)
{
  if (bar(argc) != baz(argc))
    __builtin_abort ();

  return 0;
}
