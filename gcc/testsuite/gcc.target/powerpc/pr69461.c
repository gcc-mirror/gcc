/* { dg-do compile } */
/* { dg-options "-O3 -mlra" } */

extern void _setjmp (void);
typedef struct {
  double real;
  double imag;
} Py_complex;
Py_complex a;
Py_complex fn1();
Py_complex fn2() { return fn1(); }
void fn3() {
  _setjmp();
  a = fn2();
}
