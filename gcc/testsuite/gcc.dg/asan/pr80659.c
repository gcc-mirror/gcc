/* PR sanitizer/80659 */
/* { dg-do compile } */

void
foo (int a)
{
  switch (a)
    {
    case 0:
      (int[3]) { };
      int h;
    }
}
