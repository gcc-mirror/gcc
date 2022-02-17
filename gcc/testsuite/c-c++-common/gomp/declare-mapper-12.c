/* { dg-do compile } */

struct XYZ {
  int a;
  int *b;
  int c;
};

#pragma omp declare mapper(struct XYZ t)
/* { dg-error "missing 'map' clause" "" { target c } .-1 } */
/* { dg-error "missing 'map' clause before end of line" "" { target c++ } .-2 } */

struct ABC {
  int *a;
  int b;
  int c;
};

#pragma omp declare mapper(struct ABC d) firstprivate(d.b) 
/* { dg-error "unexpected clause" "" { target c } .-1 } */
/* { dg-error "expected end of line before '\\(' token" "" { target c } .-2 } */
/* { dg-error "unexpected clause before '\\(' token" "" { target c++ } .-3 } */
