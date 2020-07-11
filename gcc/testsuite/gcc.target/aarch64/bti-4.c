/* { dg-do compile } */
/* If configured with --enable-standard-branch-protection, disable it. */
/* { dg-additional-options "-mbranch-protection=none" { target { default_branch_protection } } } */

void f1 (void *);
void f2 (void *);
void f3 (void *, void (*)(void *));

int
retbr_trampolines (void *a, int b)
{
  if (!b)
    {
      f1 (a);
      return 1;
    }
  if (b)
    {
      /* Suppress "ISO C forbids nested functions" warning. */
      _Pragma("GCC diagnostic push")
      _Pragma("GCC diagnostic ignored \"-Wpedantic\"")
      void retbr_tramp_internal (void *c)
      {
      _Pragma("GCC diagnostic pop")
        if (c == a)
          f2 (c);
      }
      f3 (a, retbr_tramp_internal);
    }
  return 0;
}

__attribute__((target("branch-protection=bti,arch=armv8.3-a")))
int
retbr_trampolines2 (void *a, int b)
{
  if (!b)
    {
      f1 (a);
      return 1;
    }
  if (b)
    {
      /* Suppress "ISO C forbids nested functions" warning. */
      _Pragma("GCC diagnostic push")
      _Pragma("GCC diagnostic ignored \"-Wpedantic\"")
      __attribute__((target("branch-protection=bti,arch=armv8.3-a")))
      void retbr_tramp_internal2 (void *c)
      {
      _Pragma("GCC diagnostic pop")
        if (c == a)
          f2 (c);
      }
      f3 (a, retbr_tramp_internal2);
    }
  return 0;
}

/* Trampoline should have BTI C. */
/* { dg-final { scan-assembler "\.LTRAMP0:\n\thint\t34" } } */
