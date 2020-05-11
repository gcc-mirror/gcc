/* { dg-options "-mgeneral-regs-only -O2" } */

struct S { float d; };

void bar (struct S);

void
f0 (int x)
{
  struct S s = { .d = 0.0f }; /* { dg-error "'-mgeneral-regs-only' is incompatible with the use of floating-point types" } */
  ((char *) &s.d)[0] = x;
  s.d *= 7.0; /* { dg-error "'-mgeneral-regs-only' is incompatible with the use of floating-point types" } */
  bar (s); /* { dg-error "'-mgeneral-regs-only' is incompatible with the use of floating-point types" } */
}
