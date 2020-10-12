/* { dg-do compile } */

/* Reject the negated form of non-negatable pragma target.  */

#pragma GCC push_options
#pragma GCC target("no-general-regs-only")

int
foo (int a)
{
  return a + 1;
}

#pragma GCC pop_options

/* { dg-error "does not allow a negated form" "" { target *-*-* } 0 } */
