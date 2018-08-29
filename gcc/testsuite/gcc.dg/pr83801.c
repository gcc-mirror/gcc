/* PR c/83801 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

static const char a[] = "01234567890123456789012345678901234567890123456789012345678901234567890123456789";
static const char b = a[27];
struct S { const char c[30]; const char d[30]; };
static const struct S e[] = { { "01234567890123456789012345678", "90123456789012345678901234567" },
                              { "89012345678901234567890123456", "78901234567890123456789012345" } };
static const char f = e[1].c[4];

char
foo (int i)
{
  return a[i];
}

char
bar (int i)
{
  return e[0].d[i];
}

/* { dg-final { scan-tree-dump {a\[i]} "original" } } */
/* { dg-final { scan-tree-dump-not {"01234567890123456789012345678901234567890123456789012345678901234567890123456789"\[i]} "original" } } */
/* { dg-final { scan-tree-dump {e\[0]\.d\[i]} "original" } } */
/* { dg-final { scan-tree-dump-not {"90123456789012345678901234567"\[i]} "original" } } */
