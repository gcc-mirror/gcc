/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-einline-optimized --param max-early-inliner-iterations=3" } */
/* { dg-add-options bind_pic_locally } */

namespace std {
  extern "C" int puts(const char *s);
}

template <class T, class E> void
foreach (T b, T e, E ptr)
{
  for (; b != e; b++)
    ptr(*b);
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
  foreach (argv, argv + argc, inline_me);
  foreach (argv, argv + argc, inline_me_too);
}

/* { dg-final { scan-tree-dump-times "Inlining void inline_me\\(" 1 "einline"} } */
/* { dg-final { scan-tree-dump-times "Inlining void inline_me_too\\(" 1 "einline"} } */
