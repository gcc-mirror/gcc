/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-einline-optimized --param max-early-inliner-iterations=5" } */
/* { dg-add-options bind_pic_locally } */

#include <algorithm>

void foo(const char *s);

void
inline_me (char *x)
{
  foo(x);
}

static void
inline_me_too (char *x)
{
  foo(x);
}

int main(int argc, char **argv)
{
  std::for_each (argv, argv + argc, inline_me);
  std::for_each (argv, argv + argc, inline_me_too);
}

/* { dg-final { scan-tree-dump-times "Inlining void inline_me\\(" 1 "einline"} } */
/* { dg-final { scan-tree-dump-times "Inlining void inline_me_too\\(" 1 "einline"} } */
