/* { dg-options "-std=gnu90" } */

void *
test_gs_to_generic (void __seg_gs *p)
{
  return p; /* { dg-error "return from pointer to non-enclosed address space" "error" } */
  /* { dg-message "expected 'void \\*' but pointer is of type '__seg_gs void \\*'" "note" { target *-*-* } .-1 } */
}

void __seg_gs *
test_generic_to_gs (void *q)
{
  return q; /* { dg-error "return from pointer to non-enclosed address space" "error" } */
  /* { dg-message "expected '__seg_gs void \\*' but pointer is of type 'void \\*'" "note" { target *-*-* } .-1 } */
}

extern void use_double_deref (char __seg_gs **buffer);

void test_double_deref (char __seg_gs *buf)
{
  use_double_deref (&buf);
}
