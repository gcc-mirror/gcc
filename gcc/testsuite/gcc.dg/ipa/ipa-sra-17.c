/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-sra -fdump-tree-optimized"  } */

#define DOIT
#define DONT


extern int extern_leaf (int);

/* ----- 1 ----- */
#ifdef DOIT
static int __attribute__((noinline))
whee_1 (int i, int j)
{
  return extern_leaf (i * j) + 1;
}

static int foo_1 (int i, int j);

static int __attribute__((noinline))
baz_1 (int i, int j)
{
  int a = 5;
  if (j)
    a = foo_1 (i, j - 1);
  return whee_1 (i, j) + a + 1;
}

static int __attribute__((noinline))
bar_1 (int i, int j)
{
  return baz_1 (i, j) + 1;
}

static int __attribute__((noinline))
foo_1 (int i, int j)
{
  return bar_1 (i, j) + 1;
}

static int __attribute__((noinline))
inter_1 (int i, int j)
{
  return foo_1 (i, j) + 1;
}
#endif

/* ----- 2 ----- */
#ifdef DONT
static int __attribute__((noinline))
whee_2 (int i, int j)
{
  return extern_leaf (i * j) + 2;
}

static int foo_2 (int i, int j);

static int __attribute__((noinline))
baz_2 (int i, int j)
{
  int a = 6;
  if (j)
    a = foo_2 (i, j - 1);
  return whee_2 (i, j) + a + 2;
}

static int __attribute__((noinline))
bar_2 (int i, int j)
{
  return baz_2 (i, j) + 2;
}

static int __attribute__((noinline))
foo_2 (int i, int j)
{
  return bar_2 (i, j) + 2;
}
#endif

/* ----- entries ----- */
#ifdef DOIT
int
entry_1 (int i, int j)
{
  inter_1 (i, j);
  return i + j + 1;
}
#endif

#ifdef DONT
int
entry_2 (int i, int j)
{
#ifdef DOIT
  inter_1 (i, j);
#endif
  return i + j + bar_2 (i, j);
}
#endif

/* { dg-final { scan-ipa-dump-times "Will remove return value" 5 "sra" } } */
/* { dg-final { scan-tree-dump-times "return;" 5 "optimized"} } */
