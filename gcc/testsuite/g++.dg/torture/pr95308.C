// { dg-do compile }
// { dg-additional-options "-march=skylake-avx512" { target x86_64-*-* i?86-*-* } }

extern int a[][18];
extern short b[], c[];
extern char d[][18];
int e;
void i(char f, long g[][100][100][100])
{
  for (int h = 0;; h += 2)
    for (char j = 0; j < 17; j++) {
	if (e ? f : 0) {
	    a[h][j] = 5;
	    for (int k = 0; k < 12; k += 4)
	      for (short l = 0; l < 015; l += 2)
		b[k * 3 + l] = bool(g[2][j][k][l]);
	} else
	  d[h][j] = 0;
	c[j] = 3;
    }
}
