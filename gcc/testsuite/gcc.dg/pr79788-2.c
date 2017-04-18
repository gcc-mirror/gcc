/* PR middle-end/79788 */
/* { dg-do compile } */
/* { dg-options "-ftrapv" } */

void bar (void);
void
foo (long long int p, long long int q)
{
  if (p >= 1234567891234567891234567891234567812 + q)	/* { dg-warning "integer constant is too large for its type" } */
    bar ();
}
