/* { dg-do compile } */
/* { dg-options "-Wfree-labels" } */

void
f (void)
{
  goto l;
 l: /* { dg-warning "label at end of compound statement" } */
}

int
g (void)
{
  goto l;
 l:
  int x = 0; /* { dg-warning "a label can only be part of a statement" } */
  return x;
}
