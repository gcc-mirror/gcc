/* { dg-do compile } */
/* { dg-options "-O -floop-nest-optimize" } */

int pc;

void
qy (int l9)
{
  int tw = 4;
  int fb[tw];

  while (l9 < 1)
    {
      int dr;

      pc = fb[2];
      for (dr = 0; dr < tw; ++dr)
	fb[dr] = 0;
      ++l9;
    }
}
