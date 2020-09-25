/* PR middle-end/89663 */
/* { dg-require-effective-target indirect_calls } */
/* { dg-require-effective-target non_strict_prototype } */

int irint (double);
long lrint (double);
long long llrint (double);
int iround (double);
long lround (double);
long long llround (double);
int iceil (double);
long lceil (double);
long long llceil (double);
int ifloor (double);
long lfloor (double);
long long llfloor (double);
int irintf (float);
long lrintf (float);
long long llrintf (float);
int iroundf (float);
long lroundf (float);
long long llroundf (float);
int iceilf (float);
long lceilf (float);
long long llceilf (float);
int ifloorf (float);
long lfloorf (float);
long long llfloorf (float);
int irintl (long double);
long lrintl (long double);
long long llrintl (long double);
int iroundl (long double);
long lroundl (long double);
long long llroundl (long double);
int iceill (long double);
long lceill (long double);
long long llceill (long double);
int ifloorl (long double);
long lfloorl (long double);
long long llfloorl (long double);

void
foo (long long *p)
{
  int (*fn) (int);
  int n = 0;
#define T(f) fn = (int (*) (int)) f; p[n++] = fn (1);
  T (irint)
  T (lrint)
  T (llrint)
  T (iround)
  T (lround)
  T (llround)
  T (iceil)
  T (lceil)
  T (llceil)
  T (ifloor)
  T (lfloor)
  T (llfloor)
  T (irintf)
  T (lrintf)
  T (llrintf)
  T (iroundf)
  T (lroundf)
  T (llroundf)
  T (iceilf)
  T (lceilf)
  T (llceilf)
  T (ifloorf)
  T (lfloorf)
  T (llfloorf)
  T (irintl)
  T (lrintl)
  T (llrintl)
  T (iroundl)
  T (lroundl)
  T (llroundl)
  T (iceill)
  T (lceill)
  T (llceill)
  T (ifloorl)
  T (lfloorl)
  T (llfloorl)
}
