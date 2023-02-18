/* This test requires special handling of a GPR which is saved because
   of -mpreserve-args but not restored.  dwarf2cfi used to ICE for
   this in maybe_record_trace_start.  The solution was to introduce a
   REG_CFA_NORESTORE reg note.  */

/* { dg-do compile } */
/* { dg-options "-O2 -march=z900 -mpreserve-args" } */

void *foo (void *);
void bar ();
int x;
void *
baz (void *y)
{
  if (__builtin_expect (x, 0))
    return foo (y);
  bar ();
  return foo (y);
}
