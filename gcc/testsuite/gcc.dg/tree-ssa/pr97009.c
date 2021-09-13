/* { dg-do run } */
/* { dg-options "-O1" } */

static int __attribute__((noipa))
get_5 (void)
{
  return 5;
}

static int __attribute__((noipa))
verify_5 (int v)
{
  if (v != 5)
    __builtin_abort ();
}

struct T
{
  int w;
  int a[4];
};

struct S
{
  int v;
  int x;
  struct T t[2];
  char alotofstuff[128];
};

volatile int vol;

void __attribute__((noipa))
consume_t (struct T t)
{
  vol = t.a[0];
}

int __attribute__((noipa))
foo (int l1, int l2)
{
  struct S s1, s2, s3;
  int i, j;

  s1.v = get_5 ();
  for (i = 0; i < l1; i++)
    {
      for (j = 0; j < l2; j++)
	s1.t[i].a[j] = get_5 ();
      consume_t(s1.t[i]);
    }

  s2 = s1;

  s3 = s2;
  for (i = 0; i < l1; i++)
    for (j = 0; j < l2; j++)
      verify_5 (s3.t[i].a[j]);
}

int
main (int argc, char *argv[])
{
  foo (2, 4);
  return 0;
}
