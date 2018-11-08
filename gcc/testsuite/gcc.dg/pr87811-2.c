/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate" } */

void bar (void);

void
foo (int i)
{
  if (__builtin_expect_with_probability (i, 0, 2.0f)) /* { dg-error "probability .* is outside the range \\\[0\\\.0, 1\\\.0\\\]" } */
    bar ();
}

/* { dg-final { scan-tree-dump-not "__builtin_expect_with_probability heuristics of edge" "profile_estimate"} } */
