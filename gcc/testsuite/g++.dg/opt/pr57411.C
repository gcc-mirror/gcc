// { dg-do compile }
// { dg-options "-O -fno-tree-dce -ftree-vectorize" }

static inline void
iota (int *__first, int *__last, int __value)
{
  for (; __first != __last; ++__first)
    {
      *__first = __value;
    }
}

void assert_fail ();

int A[] = { 0, 0, 0 };

void
test01 (int equal)
{
  iota (A, A + 3, 1);
  if (equal)
    assert_fail ();
}
