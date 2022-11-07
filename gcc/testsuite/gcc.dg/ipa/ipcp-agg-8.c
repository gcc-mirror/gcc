/* { dg-do compile } */
/* { dg-options "-O3 -fno-ipa-sra -fdump-ipa-cp"  } */
/* { dg-add-options bind_pic_locally } */

struct S
{
  int a, b, c;
};

void *blah(int, void *);

static void __attribute__ ((noinline)) foo (int x, int z, struct S *p);

static void __attribute__ ((noinline))
bar (int x, int z, struct S *p)
{
  p->b = 0;
  foo (z, x, p);
}

static void __attribute__ ((noinline))
foo (int x, int z, struct S *p)
{
  int i, c = p->c;
  int b = p->b - z;
  void *v = (void *) p;

  if (z)
    {
      z--;
      bar (z, x, p);
    }
  for (i = 0; i< c; i++)
    v = blah(b + x + i, v);
}

void
entry (int c)
{
  struct S s;
  int i;

  for (i = 0; i<c; i++)
    {
      s.a = c;
      s.b = 64;
      s.c = 32;
      foo (4, i, &s);
    }
}
/* { dg-final { scan-ipa-dump "Aggregate replacements: 1\\\[4]=64\\(by_ref\\), 1\\\[8]=32\\(by_ref\\)" "cp" } } */
/* { dg-final { scan-ipa-dump "Aggregate replacements: 1\\\[4]=0\\(by_ref\\)" "cp" } } */
