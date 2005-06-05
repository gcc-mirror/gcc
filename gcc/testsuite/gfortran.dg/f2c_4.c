extern double f2c_4b__(double *);
extern void abort (void);

void f2c_4a__(void) {
  double a,b;
  a = 1023.0;
  b=f2c_4b__(&a);
  if ( a != b ) abort();
}
