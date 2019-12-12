/* { dg-do run } */
/* { dg-options "-O2 -fipa-sra -fdump-ipa-sra"  } */

/* Check of a simple and transitive structure split. */

struct S
{
  float red;
  void *blue;
  int green;
};


void __attribute__((noipa))
check (float r, int g, int g2)
{
  if (r < 7.39 || r > 7.41
      || g != 6 || g2 != 6)
    __builtin_abort ();
}

static void
__attribute__((noinline))
foo (struct S s)
{
  check (s.red, s.green, s.green);
}

static void
__attribute__((noinline))
bar (struct S s)
{
  foo (s);
}

int
main (int argc, char *argv[])
{
  struct S s;

  s.red = 7.4;
  s.green = 6;
  s.blue = &s;

  bar (s);
  return 0;
}

/* { dg-final { scan-ipa-dump-times "Will split parameter" 2 "sra" } } */
/* { dg-final { scan-ipa-dump-times "component at byte offset" 4 "sra" } } */
