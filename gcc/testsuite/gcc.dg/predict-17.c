/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate-details" } */

extern int global;

void foo (int base)
{
  for (int i = 0; __builtin_expect_with_probability (i < base, 1, 0.05f); i++)
    global++;
}

/* { dg-final { scan-tree-dump "first match heuristics: 5.00%" "profile_estimate"} } */
/* { dg-final { scan-tree-dump "__builtin_expect_with_probability heuristics of edge .*->.*: 5.00%" "profile_estimate"} } */
/* { dg-final { scan-tree-dump "is probably executed at most 19" "profile_estimate"} } */

