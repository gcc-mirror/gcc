/* { dg-do compile } */
/* { dg-options "-O1" } */

/* This used to result in an ICE.  */

_Decimal64 y[258][258];
_Decimal64 dd[258][258];
_Decimal64 ry[258][258];
_Decimal64
foo (void)
{
  int i;
  int j;
  int m;
  int im;
  int jm;
  int ip;
  int jp;
  int i2m;
  int i1p;
  _Decimal64 a;
  _Decimal64 b;
  _Decimal64 c;
  _Decimal64 qi;
  _Decimal64 qj;
  _Decimal64 xx;
  _Decimal64 yx;
  _Decimal64 xy;
  _Decimal64 yy;
  _Decimal64 rel;
  _Decimal64 qxx;
  _Decimal64 qyy;
  _Decimal64 qxy;
  do
    {
      jp = j + 1;
      for (i = i1p; i <= i2m; i++)
	{
	  ip = i + 1;
	  yx = y[ip][j] - y[im][j];
	  yy = y[i][jp] - y[i][jm];
	  a = 0.25dd * (xy * xy + yy * yy);
	  b = 0.25dd * (xx * xx + yx * yx);
	  c = 0.125dd * (xx * xy + yx * yy);
	  qj = 0.0dd;
	  dd[i][m] = b + a * rel + b;
	  qxx = y[ip][j] - 2.0dd * y[i][j] + y[im][j];
	  qyy = y[i][jp] - 2.0dd * y[i][j] + y[i][jm];
	  qxy = y[ip][jp] - y[ip][jm] - y[im][jp] + y[im][jm];
	  ry[i][m] = a * qxx + b * qyy - c * qxy + yx * qi + yy * qj;
	}
    }
  while (1);
}
