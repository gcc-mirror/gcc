/* { dg-do compile } */
/* { dg-options "-fdump-tree-profile_estimate" } */

void bar (void);

void
foo (int i)
{
  if (__builtin_expect_with_probability (i, 0, 2.0f))
    bar ();
}
