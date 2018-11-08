/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-einline-optimized --param max-early-inliner-iterations=2" } */
/* { dg-add-options bind_pic_locally } */

extern void inlined ();
void inline_me_too (void);
void inline_through_me (void (*ptr)(void));
void
inline_me (void)
{
  inlined();
}

void main(void)
{
  inline_through_me (inline_me);
  inline_through_me (inline_me_too);
}
void
inline_through_me (void (*ptr)(void))
{
  ptr();
}

void
inline_me_too (void)
{
  inlined();
}
/* { dg-final { scan-tree-dump-times "Inlining inline_me/\[0-9\]* " 1 "einline"} } */
/* { dg-final { scan-tree-dump-times "Inlining inline_me_too/\[0-9\]* " 1 "einline"} } */
