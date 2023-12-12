// { dg-do compile }
// { dg-require-effective-target vect_int }

inline const int& maxx (const int& a, const int &b)
{
  return a > b ? a : b;
}

int foo(int *a)
{
  int max = 0;
  for (int i = 0; i < 1024; ++i)
    max = maxx(max, a[i]);
  return max;
}

// { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" { xfail vect_no_int_min_max } } }
