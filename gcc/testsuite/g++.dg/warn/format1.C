// Test that formats get checked according to C94.
// Origin: Joseph Myers <jsm28@cam.ac.uk>.
// { dg-do compile }
// { dg-options "-ansi -pedantic -Wformat" }

#include <cstdio>

void
foo (int i, int *ip, __WINT_TYPE__ lc, wchar_t *ls)
{
  std::printf ("%d%ls%lc\n", i, ls, lc);
  std::printf ("%d", ls); // { dg-warning "format" "printf warning" }
  std::scanf ("%d%lc%ls%l[abc]", ip, ls, ls, ls);
  std::scanf ("%hd", ip); // { dg-warning "format" "scanf warning" }
}
