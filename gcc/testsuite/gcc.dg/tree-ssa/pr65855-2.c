/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-sccp" } */

int square(int x) {
  int result = 0;
  for (int i = 0; i < x; ++i)
    result += x;
  return result;
}

/* { dg-final { scan-tree-dump " with expr: x_\[0-9\]\\(D\\) \\* x_\[0-9\]\\(D\\)" "sccp" } } */
