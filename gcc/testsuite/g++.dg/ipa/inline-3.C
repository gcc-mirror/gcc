/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-inline -fno-ipa-icf --param max-early-inliner-iterations=1" } */
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

/* { dg-final { scan-ipa-dump-times "Considering void inline_me\\(" 1 "inline"} } */
/* { dg-final { scan-ipa-dump-times "Considering void inline_me_too\\(" 1 "inline"} } */
