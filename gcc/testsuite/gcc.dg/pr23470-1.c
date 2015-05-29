/* PR middle-end/23470 */
/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -fdump-tree-original" } */

void link_error (void);

int f(double a, double b)
{
  if (((a*a) + (b*b))<0)
    link_error();
}

/* { dg-final { scan-tree-dump-times "if \\(0\\)" 1 "original" } } */
