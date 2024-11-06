// { dg-do compile }

short a, b, c;
unsigned d(unsigned, int e) { return e; }
void f(bool g, short e[][3][3][3][3], unsigned h[][3][3], char i[][8],
       short j[][18][18][18], short k[][18][18][18], short l[][8][8][8][8])
{
  for (char m;;)
    {
      for (short n = 0; n < 8; n += 5)
	a = j[m][6][2][m];
      for (short o(l[m][m][m][m][m] / i[m][m] ?: e[m][m][4][m][2]); o; o = g)
	for (char p; p < (c && i[g]) + 7; p += 2)
	  b = d(h[6][g][2], k[m][5][g][2] != m);
    }
}
