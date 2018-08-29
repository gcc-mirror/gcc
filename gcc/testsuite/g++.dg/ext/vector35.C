// PR c++/85077
// { dg-do compile }
// { dg-options "-Ofast -fdump-tree-forwprop1" }

typedef float V __attribute__((vector_size (4 * sizeof (float))));
typedef double W __attribute__((vector_size (2 * sizeof (double))));

void
foo (V *y)
{
  V x = *y;
  *y = x < 0 ? -x : x;
}

void
bar (W *y)
{
  W x = *y;
  *y = x < 0 ? -x : x;
}

// { dg-final { scan-tree-dump-times "ABS_EXPR <" 2 "forwprop1" } }
