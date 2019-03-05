/* { dg-do compile } */
/* { dg-options "-O2 -fopt-info-inline --param max-early-inliner-iterations=3" } */
/* { dg-add-options bind_pic_locally } */

namespace std {
  extern "C" int puts(const char *s);
}

template <class T, class E> void
foreach (T b, T e, void (*ptr)(E))
{
  for (; b != e; b++)
    ptr(*b); // { dg-optimized "Inlining void inline_me\[^\\n\]* into int main\[^\\n\]*" }
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
  foreach (argv, argv + argc, inline_me); // { dg-optimized "Inlining void foreach\[^\\n\]* into int main\[^\\n\]*" }
  foreach (argv, argv + argc, inline_me_too); // { dg-optimized "Inlining void foreach\[^\\n\]* into int main\[^\\n\]*" }
}
