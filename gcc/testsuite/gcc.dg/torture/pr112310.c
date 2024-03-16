/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

extern void abort (void);
short a, b;
static int c, d, e, f = -7;
char g;
int *h = &d;
int **i = &h;
int j;
short(k)(int l) { return l >= 2 ? 0 : l; }
int(m)(int l, int n) { return l < -2147483647 / n ? l : l * n; }
void o() { &c; }
int main()
{
  int p;
  for (; g <= 3; g++)
    {
      for (; c; c++)
	;
      a = 2;
      for (; a <= 7; a++) {
	  short *r = &b;
	  p = m(*h, 2022160547);
	  unsigned q = 2022160547;
	  e = p * q;
	  *r ^= e;
	  j = k(c + 3);
	  **i = 0;
      }
      *i = &f;
    }
  if (b != -3189)
    abort ();
  return 0;
}
