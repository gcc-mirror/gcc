/* PR c/49859 */
/* { dg-do compile } */
/* { dg-options "-Wswitch-unreachable" } */

extern void foo (int);
extern int j;

void
fn0 (int i)
{
  switch (i)
    {
    int t = 10; /* { dg-warning "statement will never be executed" } */
    default:
      foo (t);
    }

  switch (i)
    { /* { dg-warning "statement will never be executed" } */
      int A[i];
      default: /* { dg-error "switch jumps into scope" } */
	break;
    }

  switch (i)
    default:
      j = sizeof (struct { int i; });

  switch (i)
    {
      int A[3];
      default:
	break;
    }
}
