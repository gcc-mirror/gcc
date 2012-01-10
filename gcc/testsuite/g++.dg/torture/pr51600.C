template<class T> inline T min(T a, T b) { return a < b ? a : b; }
double cornerbound(double *P, double (*m)(double, double))
{
  double b=m(P[0],P[3]);
  return m(b,P[12]);
}
void bound(double *P, double (*m)(double, double), double b)
{
  m(b,cornerbound(P,m));
}
void bounds(double fuzz, unsigned maxdepth)
{
  double Px[]={};
  double bx=Px[0];
  bound(Px,min,bx);
}
