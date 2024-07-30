/* Make sure s390_check_qrst_address () correctly handles UNSPEC rtxs.  */
/* { dg-do compile } */
/* { dg-options "-march=z196 -O2 -fPIC" } */

int a, b, g, h;
struct
{
  int i;
  struct
  {
    int d;
  } k[];
} f;

void m(int);

void l()
{
  int j;
  for (; h;)
    {
      m((
	{
	  __asm__("" : "=r"(g));
	  b;
	}
      ));
      f.k[j].d = a;
      j++;
    }
  f.i = j;
}
