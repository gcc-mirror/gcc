/* Accesses to complex numbers were sometimes marked as scalar and
   sometimes as struct accesses.  */
/* { dg-do run } */
/* { dg-options "-std=c99" } */

extern void abort (void);
static double _Complex *fp_cxd(double _Complex *cx) {
  return cx;
}

int main( ) {
  double _Complex cx = 4.0 + 3.0*(__extension__ 1.0iF);
  double _Complex cx43 = 4.0 + 3.0*(__extension__ 1.0iF);
  double _Complex cx11 = 1.0 + 1.0*(__extension__ 1.0iF);

  *fp_cxd(&cx) *= cx11;
  *fp_cxd(&cx) /= cx11;

  double r_cx = __real__(cx);
  double i_cx = __imag__(cx);
  double r_cx43 = __real__(cx43);
  double i_cx43 = __imag__(cx43);

  if( (r_cx == r_cx43) && (i_cx == i_cx43) ) { 
    return 0;
  } else {
    abort ();
  }
}
