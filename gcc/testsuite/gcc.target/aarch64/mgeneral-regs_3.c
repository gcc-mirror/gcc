/* { dg-options "-mgeneral-regs-only -O2" } */

extern void abort (void);

int
test (int i, ...)
{
  float f = (float) i; /* { dg-error "'-mgeneral-regs-only' is incompatible with the use of floating-point types" } */
  if (f != f) abort ();
  return 2;
}
