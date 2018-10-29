/* { dg-do compile } */
/* { dg-require-effective-target int32plus } */

int b;
int d;
void e()
{
  unsigned f;
  unsigned g;
  int h;
  long i = 901380;
  for (;;) {
      d = 0;
      for (; d; d++) {
	  h = 143366620;
	  f = 0;
	  for (; f < 15; f += 3) {
	      g = 0;
	      for (; g < 9; g++)
		b = h = i - (h << 5) + h;
	  }
      }
      i = 0;
  }
}
