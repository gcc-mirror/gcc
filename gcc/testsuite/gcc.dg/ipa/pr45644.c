/* Verify that we do not IPA-SRA bitfields.  */
/* { dg-do run } */
/* { dg-options "-O2"  } */
/* { dg-require-effective-target int32plus } */

extern void abort (void);

struct S
{
  int j : 8;
  int i : 24;
  int l;
};

static int __attribute__((noinline)) foo (struct S *s)
{
  int z = s->i;
  if (z != 777)
    abort ();
  return 0;
}

int __attribute__((noinline)) bar (struct S *s)
{
  return foo (s);
}

int main (int argc, char *argv[])
{
  struct S s;
  s.j = 5;
  s.i = 777;
  s.l = -1;

  return bar (&s);
}
