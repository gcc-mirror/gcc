/* { dg-options "-mgeneral-regs-only -O2" } */

extern void abort (void);

int
test (int i, ...) /* { dg-error "'-mgeneral-regs-only' is incompatible with the use of floating-point types" } */
{
  float f = (float) i;
  if (f != 0) abort ();
  return 2;
}
