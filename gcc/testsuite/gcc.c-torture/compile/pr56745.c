/* PR rtl-optimization/56745 */

unsigned char a[6];

void
foo ()
{
  int i;
  for (i = 5; i >= 0; i++)
    {
      if (++a[i] != 0)
	break;
      ++a[i];
    }
}
