/* { dg-do compile } */

struct g {
    char b;
    unsigned char *d[];
} e;
unsigned char f;
void i()
{
  for (int h;; h++)
    {
      unsigned a = h;
      for (int c = 0; c < 256; c += 6)
	for (int h = c; h < c + 6; h++)
	  e.d[h + a] = &f;
    }
}
