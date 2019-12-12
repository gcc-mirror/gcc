/* { dg-do compile } */
/* { dg-options "-O2 -fipa-sra -fdump-ipa-sra -fdump-tree-optimized"  } */

/* Testing removal of unused parameters in recursive calls.  */

extern int work_1 (int);
extern int work_2 (int);

static int __attribute__((noinline))
foo (int l, int w1, int w2, int useless, int useless2);


static int __attribute__((noinline))
bar_1 (int l, int w1, int w2, int useless, int useless2)
{
  return work_1 (w1) + foo (l, w1, w2, useless2, useless);
}

static int __attribute__((noinline))
baz_1 (int useless, int useless2, int l, int w1, int w2)
{
  return bar_1 (l, w1, w2, useless, useless2);
}

static int __attribute__((noinline))
bax_1 (int l, int w1, int w2, int useless, int useless2)
{
  return baz_1 (useless, useless2, l, w1, w2);
}



static int __attribute__((noinline))
bar_2 (int l, int w1, int w2, int useless, int useless2)
{
  return foo (l, w1, w2, useless2 + 5, useless);
}

static int __attribute__((noinline))
baz_2 (int useless, int useless2, int l, int w1, int w2)
{
  return bar_2 (l, w1, w2, useless, useless2);
}


static int __attribute__((noinline))
bax_2 (int l, int w1, int w2, int useless, int useless2)
{
  return work_2 (w2) + baz_2 (useless, useless2, l, w1, w2);
}


static int __attribute__((noinline))
 foo (int l, int w1, int w2, int useless, int useless2)
{
  int r = 0;
  if (!l)
    return r;
  if (l % 2)
    r = bax_1 (l - 1, w1, w2, useless, useless2);
  else
    r = bax_2 (l - 1, w1, w2, useless, useless2);

  return r;
}

int
entry (int l, int w1, int w2, int noneed, int noneed2)
{
  return foo (l, w2, w2, noneed2, noneed2 + 4);
}

/* { dg-final { scan-ipa-dump-times "Will remove parameter" 14 "sra" } } */
/* { dg-final { scan-tree-dump-not "useless" "optimized"} } */
