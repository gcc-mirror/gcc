/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop1" } */

typedef __SIZE_TYPE__ size_t;
typedef __PTRDIFF_TYPE__ ptrdiff_t;

ptrdiff_t foo1 (char *a, size_t n)
{
  char *b1 = a + 8 * n;
  char *b2 = a + 8 * (n - 1);

  return b1 - b2;
}

int use_ptr (char *a, char *b);

ptrdiff_t foo2 (char *a, size_t n)
{
  char *b1 = a + 8 * (n - 1);
  char *b2 = a + 8 * n;

  use_ptr (b1, b2);

  return b1 - b2;
}

int use_int (int i);

unsigned goo (unsigned m_param, unsigned n_param)
{
  unsigned b1 = m_param * (n_param + 2);
  unsigned b2 = m_param * (n_param + 1);
  int r = (int)(b1) - (int)(b2);

  use_int (r);

  return r;
}

/* { dg-final { scan-tree-dump-times "return 8;" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "return -8;" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "return m_param" 1 "forwprop1" } } */
