/* { dg-do run } */
/* { dg-options "-O2" } */

void abort (void);

struct S2848
{
  unsigned int a;
  _Complex int b;
};

struct S2848 s2848;

void __attribute__((noinline))
check2848 (struct S2848 arg0)
{
  if (arg0.b != s2848.b)
    abort ();
}

int main()
{
  s2848.a = 4027477739U;
  s2848.b = (723419448 + -218144346 * __extension__ 1i);

  check2848 (s2848);

  return 0;
}
