/* PR124528 */
/* { dg-options "-Os -fdump-tree-sccp" } */

int baz (int n, int m)
{
  int r = 100;
  while (n != 0) {
    n = n - 1;
    r = r - m;
  }
  return r;
}

/* Make sure we negate m in unsigned.  */
/* { dg-final { scan-tree-dump "\\\(unsigned int\\\) m_.\\\(D\\\)" "sccp" } } */
