// { dg-do compile }
// { dg-require-effective-target vect_int }

static int op (const int& x, const int& y) { return x + y; }

void foo(int* a)
{
  for (int i = 0; i < 1000; ++i)
    a[i] = op(a[i], 1);
}

// { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } }
