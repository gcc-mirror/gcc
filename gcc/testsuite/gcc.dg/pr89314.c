/* PR tree-optimization/89314 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

extern __SIZE_TYPE__ strlen (const float *);
void bar (void);

void
foo (float *s)
{
  if (strlen (s) > 0)
    bar ();
}
