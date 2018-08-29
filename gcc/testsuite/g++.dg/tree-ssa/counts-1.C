/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
void foo();
extern void abort (void);

static __attribute__ ((noinline))
void mark_me_unlikely ()
{
  foo();
  foo();
  foo();
  foo();
}

void i_am_not_unlikely()
{
  try { foo(); }
  catch (int) {mark_me_unlikely ();}
}
/* { dg-final { scan-tree-dump "mark_me_unlikely\[^\r\n\]*(unlikely executed)" "optimized"} } */
/* { dg-final { scan-tree-dump-not "i_am_not_unlikely\[^\r\n\]*(unlikely executed)" "optimized"} } */
