/* Test for GNU extensions to C99 designated initializers */
/* Origin: Jakub Jelinek <jakub@redhat.com> */
/* { dg-do run } */
/* { dg-options "-std=gnu99" } */

typedef __SIZE_TYPE__ size_t;
extern int memcmp (const void *, const void *, size_t);
extern void abort (void);
extern void exit (int);

int a[][2][4] = { [2 ... 4][0 ... 1][2 ... 3] = 1, [2] = 2, [2][0][2] = 3 };
struct E {};
struct F { struct E H; };
struct G { int I; struct E J; int K; };
struct H { int I; struct F J; int K; };
struct G k = { .J = {}, 1 };
struct H l = { .J.H = {}, 2 };
struct H m = { .J = {}, 3 };
struct I { int J; int K[3]; int L; };
struct M { int N; struct I O[3]; int P; };
struct M n[] = { [0 ... 5].O[1 ... 2].K[0 ... 1] = 4, 5, 6, 7 };
struct M o[] = { [0 ... 5].O = { [1 ... 2].K[0 ... 1] = 4 },
		 [5].O[2].K[2] = 5, 6, 7 };
struct M p[] = { [0 ... 5].O[1 ... 2].K = { [0 ... 1] = 4 },
		 [5].O[2].K[2] = 5, 6, 7 };
int q[3][3] = { [0 ... 1] = { [1 ... 2] = 23 }, [1][2] = 24 };
int r[1] = { [0 ... 1 - 1] = 27 };

int main (void)
{
  int x, y, z;

  if (a[2][0][0] != 2 || a[2][0][2] != 3)
    abort ();
  a[2][0][0] = 0;
  a[2][0][2] = 1;
  for (x = 0; x <= 4; x++)
    for (y = 0; y <= 1; y++)
      for (z = 0; z <= 3; z++)
	if (a[x][y][z] != (x >= 2 && z >= 2))
	  abort ();
  if (k.I || l.I || m.I || k.K != 1 || l.K != 2 || m.K != 3)
    abort ();
  for (x = 0; x <= 5; x++)
    {
      if (n[x].N || n[x].O[0].J || n[x].O[0].L)
	abort ();
      for (y = 0; y <= 2; y++)
	if (n[x].O[0].K[y])
	  abort ();
      for (y = 1; y <= 2; y++)
	{
	  if (n[x].O[y].J)
	    abort ();
	  if (n[x].O[y].K[0] != 4)
	    abort ();
	  if (n[x].O[y].K[1] != 4)
	    abort ();
	  if ((x < 5 || y < 2) && (n[x].O[y].K[2] || n[x].O[y].L))
	    abort ();
	}
      if (x < 5 && n[x].P)
	abort ();
    }
  if (n[5].O[2].K[2] != 5 || n[5].O[2].L != 6 || n[5].P != 7)
    abort ();
  if (memcmp (n, o, sizeof (n)) || sizeof (n) != sizeof (o))
    abort ();
  if (memcmp (n, p, sizeof (n)) || sizeof (n) != sizeof (p))
    abort ();
  if (q[0][0] || q[0][1] != 23 || q[0][2] != 23)
    abort ();
  if (q[1][0] || q[1][1] != 23 || q[1][2] != 24)
    abort ();
  if (q[2][0] || q[2][1] || q[2][2])
    abort ();
  if (r[0] != 27)
    abort ();
  exit (0);
}
