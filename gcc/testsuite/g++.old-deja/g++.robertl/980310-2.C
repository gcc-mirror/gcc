// float mismatch.  Abors on i586-pc-linux-gnulibc1 with egcs 1.0.1
// Should run smoothly. Aborts on i586-pc-linux-gnulibc1
// From: Max Lawson <mlawson@drfmc.ceng.cea.fr>
// Message-Id: <9803091022.AA07520@drfmc.ceng.cea.fr>

#include <cstdlib>

void f(double j, double& v)
{
  size_t sz = size_t(2*j+1);
  double norm_ = j*(j+1);
  double m = j;
  int sign_ = -1;
  for (size_t c=1;c<=sz;++c)
    for (size_t r=1;r<=sz;++r)
      if (r+sign_*1 == c)
        {
          double val = (norm_-m*(m+sign_));
          for (size_t k=1;k<2;++k) 
            val *= (norm_ - (m+sign_*k)*(m+sign_*(k+1)));
          v = val;
        }
}

int main()
{
  double v;
  f(1,v);
  if (v != 4) abort();

  return 0;
}

