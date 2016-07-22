/* { dg-options "-Wno-psabi" } */
/* { dg-require-effective-target int32plus } */
void abort (void);

struct S2848
{
  unsigned int a;
  _Complex int b;
  struct
  {
  } __attribute__ ((aligned)) c;
};

struct S2848 s2848;

int fails;

void  __attribute__((noinline))
check2848va (int z, ...)
{
  struct S2848 arg;
  __builtin_va_list ap;

  __builtin_va_start (ap, z);

  arg = __builtin_va_arg (ap, struct S2848);

  if (s2848.a != arg.a)
    ++fails;
  if (s2848.b != arg.b)
    ++fails;

  __builtin_va_end (ap);
}

int main (void)
{
  s2848.a = 4027477739U;
  s2848.b = (723419448 + -218144346 * __extension__ 1i);

  check2848va (1, s2848);

  if (fails)
    abort ();

  return 0;
}

