/* { dg-require-effective-target float128 } */
/* { dg-options "-O2 -mdejagnu-cpu=power9 -fselective-scheduling2" } */

/* Verify there is no ICE.  */

int a[10];
int b(_Float128 e) {
  int c;
  _Float128 d;
  c = e;
  d = c;
  d = a[c] + d;
  return d;
}
