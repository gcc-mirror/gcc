/* PR tree-optimization/71994 */
/* { dg-do compile } */
int om, h6;

void eo (void)
{
  const int tl = 1;
  int ln;

  h6 = (om + tl) > 0;
  ln = om && (om & h6);
  h6 = om;
  om = ln < h6;
}
