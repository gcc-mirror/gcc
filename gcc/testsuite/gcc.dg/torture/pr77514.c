/* { dg-do compile } */

void
m1 (char l0, char e8, int hw)
{
  char *rs = &l0;

yu:
  l0 = 1;
  while (l0 != 0)
    {
      l0 = -l0;
      l0 += (*rs ^ (l0 &= 1));
    }
  for (;;)
    {
      if (hw != 0)
	goto yu;
      rs = &e8;
    }
}
