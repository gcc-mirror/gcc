/* PR tree-optimization/89314 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wbuiltin-declaration-mismatch -Wextra" } */

extern __SIZE_TYPE__ strlen (const float *);	/* { dg-warning "mismatch in argument 1 type of built-in function" } */
void bar (void);

void
foo (float *s)
{
  if (strlen (s) > 0)
    bar ();
}
