/* { dg-do compile } */
/* { dg-options "-O3 -fdump-ipa-cp-details -fno-early-inlining --param ipa-cp-max-recursive-depth=8 --param ipa-cp-eval-threshold=1" } */

struct V {
  int f0;
  int f1;
};

int data[100];

int fn ();

int recur_fn (struct V * __restrict v)
{
  int i = v->f0;
  int j = v->f1;
  struct V t;

  if (j > 100)
    {
      fn ();
      return 1;
    }

  data[i] = i;

  t.f0 = i - 2;
  t.f1 = j + 1;

  recur_fn (&t);

  return i * j;
}

int main ()
{
  struct V v;

  v.f0 = 1;
  v.f1 = 3;
  return recur_fn (&v);
}

/* { dg-final { scan-ipa-dump-times "Creating a specialized node of recur_fn/\[0-9\]*\\." 8 "cp" } } */
