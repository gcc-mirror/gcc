/* PR target/8232.  */

int f (char *p, char *q, int i)
{
  return bcmp (p, q, i);
}

