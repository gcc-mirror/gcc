/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate" } */

extern int global;

void foo (int base)
{
  if (__builtin_expect_with_probability (base == 100, 1, 0.001f))
    global++;
}

/* { dg-final { scan-tree-dump "first match heuristics: 0.10%" "profile_estimate"} } */
/* { dg-final { scan-tree-dump "__builtin_expect_with_probability heuristics of edge .*->.*: 0.10%" "profile_estimate"} } */
