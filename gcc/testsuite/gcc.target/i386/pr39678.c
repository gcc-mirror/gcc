/* PR target/39678 */
/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2" } */

struct X {
  char c;
  __complex__ float val;
};

struct X
foo (float *p)
{ /* { dg-message "note: The ABI of passing structure with complex float member has changed in GCC 4.4" } */
  struct X x;
  x.c = -3;
  __real x.val = p[0];
  __imag x.val = p[1];
  return x;
}
