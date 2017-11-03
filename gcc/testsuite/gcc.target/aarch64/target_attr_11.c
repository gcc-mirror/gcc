/* { dg-do compile } */

/* Reject the negated form of non-negatable attributes.  */

__attribute__ ((target ("no-general-regs-only")))
int
foo (int a)
{
  return a + 1;
}

/* { dg-error "does not allow a negated form" "" { target *-*-* } 0 } */
/* { dg-error "is not valid" "" { target *-*-* } 0 } */
