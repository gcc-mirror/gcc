/* Test cases of structures with volatile fields declared register:
   should be allowed unless register name given but explicitly taking
   the address forbidden.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */

/* { dg-do compile } */

struct S { volatile int field; };

void
f (void)
{
  register struct S a;
  register struct S b[2];
  register struct S c __asm__("nosuchreg"); /* { dg-error "object with volatile field" "explicit reg name" } */
  &a; /* { dg-warning "address of register" "explicit address" } */
  b; /* { dg-warning "address of register" "implicit address" } */
}
