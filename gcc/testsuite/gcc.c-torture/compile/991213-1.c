typedef __complex__ double double_complex;



void
p (const double_complex *t, int n)
{
  int i;
  double s = ({ typeof ( t[n/2] ) arg = ( t[n/2] ); (__imag__ arg); }) ;
  for (i = 1; i < n/2; i++)
    s += 2* ({ typeof ( t[i] ) arg = ( t[i] ); (__imag__ arg); }) ;
}

