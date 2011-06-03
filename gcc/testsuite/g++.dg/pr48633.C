/* { dg-do compile} */
/* { dg-options "-O2 -fira-region=all -fnon-call-exceptions" } */
extern long double getme (void);
extern void useme (long double);
struct Frame {
    long double tmp;
};
void bugme (int n, long double ld1, long double ld2, long double ld3,
            long double ld4, long double ld5)
{
  Frame f;
  int i;
  f.tmp = getme();
  try {
    for (i = 0; i < n; i++)
      {
        f.tmp += 1.0;
      }
  } catch (...) {
    f.tmp += 1.0;
  }
  ld1++;
  ld2++;
  ld3++;
  ld4++;
  ld5++;
  useme (f.tmp);
}
