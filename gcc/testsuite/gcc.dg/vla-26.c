/* { dg-do compile } */
/* { dg-options "-std=c99 -O2" } */

void ed(int n, float s[3][n])
{
	for (int i = 0; i < n; i++)
		s[1][i];
}

void e(int n, float s[3][n])
{
	ed(n, s);	
}


