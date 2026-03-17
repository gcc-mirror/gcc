/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate" } */
/* PR tree-optimization/117935 */

static inline bool has_value(int b)
{
  if (b)
  {
    [[gnu::hot, gnu::unused]] label1:
    return true;
  }
  else
    return false;
}
/* The hot label should last until it gets inlined into value_or and jump_threaded.  */
int value_or(int b, int def0, int def1)
{
  if (has_value(b))
    return def0;
  else
    return def1;
}
/* { dg-final { scan-tree-dump-times "first match heuristics: 90.00%" 2 "profile_estimate"} } */
/* { dg-final { scan-tree-dump-times "hot label heuristics of edge \[0-9\]+->\[0-9]+: 90.00%" 2 "profile_estimate"} } */
