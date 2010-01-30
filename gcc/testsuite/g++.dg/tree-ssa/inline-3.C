/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-einline2" } */
/* { dg-add-options bind_pic_locally } */

#include <algorithm>

namespace std {
  extern "C" void puts(const char *s);
}

void
inline_me (char *x)
{
  std::puts(x);
}

static void
inline_me_too (char *x)
{
  std::puts(x);
}

int main(int argc, char **argv)
{
  std::for_each (argv, argv + argc, inline_me);
  std::for_each (argv, argv + argc, inline_me_too);
}

/* { dg-final { scan-tree-dump-times "Inlining void inline_me\\(" 1 "einline2"} } */
/* { dg-final { scan-tree-dump-times "Inlining void inline_me_too\\(" 1 "einline2"} } */
/* { dg-final { cleanup-tree-dump "einline2" } } */
