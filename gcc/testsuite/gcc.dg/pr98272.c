/* PR tree-optimization/98272 */
/* Reported by Zdenek Sojka <zsojka@seznam.cz> */

/* { dg-do compile } */
/* { dg-options "-O -fno-tree-forwprop" } */

void bar (void);

void
foo (unsigned char uc)
{
  if (uc >= 5)
    return;

  switch (uc)
    {
    case 0:
    case 2:
    case 4:
      bar ();
    }
}
