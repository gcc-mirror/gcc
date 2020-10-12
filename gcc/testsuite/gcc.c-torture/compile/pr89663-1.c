/* PR middle-end/89663 */
/* { dg-require-effective-target non_strict_prototype } */

int irint ();
long lrint ();
long long llrint ();
int iround ();
long lround ();
long long llround ();
int iceil ();
long lceil ();
long long llceil ();
int ifloor ();
long lfloor ();
long long llfloor ();
int irintf ();
long lrintf ();
long long llrintf ();
int iroundf ();
long lroundf ();
long long llroundf ();
int iceilf ();
long lceilf ();
long long llceilf ();
int ifloorf ();
long lfloorf ();
long long llfloorf ();
int irintl ();
long lrintl ();
long long llrintl ();
int iroundl ();
long lroundl ();
long long llroundl ();
int iceill ();
long lceill ();
long long llceill ();
int ifloorl ();
long lfloorl ();
long long llfloorl ();

void
foo (long long *p)
{
  int n = 0;
#define T(f) p[n++] = f (1);
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
