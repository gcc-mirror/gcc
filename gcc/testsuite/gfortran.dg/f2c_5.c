extern float f2c_5b_(double *);
extern void abort (void);

void f2c_5a_(void) {
  double a,b;
  a = 1023.0;
  b=f2c_5b_(&a);
  if ( a != b ) abort();
}
