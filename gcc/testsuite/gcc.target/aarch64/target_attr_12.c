/* { dg-do compile } */

/* Reject arguments to attributes that do not accept any.  */

__attribute__ ((target ("general-regs-only=+crc")))
int
foo (int a)
{
  return a + 1;
}

/* { dg-error "does not accept an argument" "" { target *-*-* } 0 } */
/* { dg-error "is not valid" "" { target *-*-* } 0 } */
