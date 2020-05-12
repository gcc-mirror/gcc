/* { dg-do run } */

int a, c, f;
long b;
char d;
int e[3];
int g[9][3][2];
int main()
{
    {
h:
      for (f = 0; f <= 5; f++) {
	  b = 3;
	  for (; b >= 0; b--) {
	      e[2] = d = 0;
	      for (; d <= 3; d++) {
		  g[8][2][0] = e[1] = c = 0;
		  for (; c <= 1; c++)
		    e[c + 1] = g[d + 5][2][c] = 4;
	      }
	      if (a)
		goto h;
	  }
      }
    }
  if (e[2] != 4)
    __builtin_abort ();
  return 0;
}
