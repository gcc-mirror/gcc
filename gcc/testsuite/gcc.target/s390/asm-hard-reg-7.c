/* { dg-do compile } */
/* { dg-options "-march=z13" } */

/* Test register pairs.  */

void
test (void)
{
  register double f0 __asm__ ("f0");
  register double f2 __asm__ ("f2");
  register long double f0f2 __asm__ ("f0");
  double x;
  long double y;

  /* Outputs */
  __asm__ __volatile__ ("" : "=r" (f0), "=r" (f0f2));
  __asm__ __volatile__ ("" : "=r" (f0f2), "={f0}" (y));  /* { dg-error "multiple outputs to hard register: %f0" } */
  __asm__ __volatile__ ("" : "={f0}" (x), "=r" (f0f2));  /* { dg-error "multiple outputs to hard register: %f0" } */

  __asm__ __volatile__ ("" : "=r" (f2), "=r" (f0f2));
  __asm__ __volatile__ ("" : "={f2}" (x), "={f0}" (y));  /* { dg-error "multiple outputs to hard register: %f2" } */
  __asm__ __volatile__ ("" : "=r" (f2), "={f0}" (y));  /* { dg-error "multiple outputs to hard register: %f2" } */
  __asm__ __volatile__ ("" : "={f2}" (x), "=r" (f0f2));  /* { dg-error "multiple outputs to hard register: %f2" } */

  /* Inputs */
  __asm__ __volatile__ ("" :: "r" (f0), "r" (f0f2));
  __asm__ __volatile__ ("" :: "r" (f0f2), "{f0}" (y));  /* { dg-error "multiple inputs to hard register: %f0" } */
  __asm__ __volatile__ ("" :: "{f0}" (x), "r" (f0f2));  /* { dg-error "multiple inputs to hard register: %f0" } */

  __asm__ __volatile__ ("" :: "r" (f2), "r" (f0f2));
  __asm__ __volatile__ ("" :: "{f2}" (x), "{f0}" (y));  /* { dg-error "multiple inputs to hard register: %f2" } */
  __asm__ __volatile__ ("" :: "r" (f2), "{f0}" (y));  /* { dg-error "multiple inputs to hard register: %f2" } */
  __asm__ __volatile__ ("" :: "{f2}" (x), "r" (f0f2));  /* { dg-error "multiple inputs to hard register: %f2" } */
}
