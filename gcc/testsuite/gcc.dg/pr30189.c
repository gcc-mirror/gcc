/* { dg-do compile } */
/* { dg-options "-g -O" } */

extern void foo (void);

static
void baz (int i)
{
  foo ();
  typedef char A[i];
  struct { A b; } *x = 0;
}

void
bar (i)
{
  baz (i);
}
