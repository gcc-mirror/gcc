/* PR tree-optimization/72866 */
/* { dg-do compile } */

unsigned int dl;
int rx, lb;

void
fo (int jv, int be)
{
  const unsigned int xw = 16;
  unsigned int ya, wo;

  for (ya = 0; ya < 2; ++ya)
    for (wo = 0; wo < xw; ++wo)
      {
	dl += (jv ? be : rx);
	rx += ((lb == 0) + 1);
      }
}
