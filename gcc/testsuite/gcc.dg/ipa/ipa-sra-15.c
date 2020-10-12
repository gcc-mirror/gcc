/* { dg-do run } */
/* { dg-options "-O2 -fipa-sra -fdump-ipa-sra"  } */

/* Check of a recursive by-reference structure split.  The recursive functions
   have to be pure right from the start, otherwise the current AA would detect
   possible modification of data.  */

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
  return;
}

static int __attribute__((noinline, pure)) bar (struct S *s, int rec);

static int
__attribute__((noinline, pure))
foo (struct S *s , int rec)
{
  int t = 0;
  if (rec)
    t = bar (s, 0);
  check (s->red, s->green, s->green);
  return t;
}

static int
__attribute__((noinline, pure))
bar (struct S *s, int rec)
{
  int t = foo (s, rec);
  return t + t;
}

volatile int g;

int
main (int argc, char **argv)
{
  struct S s;

  s.red = 7.4;
  s.green = 6;
  s.blue = &s;

  g = bar (&s, 1);
  return 0;
}

/* { dg-final { scan-ipa-dump-times "Will split parameter" 2 "sra" } } */
/* { dg-final { scan-ipa-dump-times "component at byte offset" 4 "sra" } } */
