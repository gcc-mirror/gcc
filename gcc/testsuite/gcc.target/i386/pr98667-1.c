/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -fcf-protection -march=i486" } */

void
test (void)
{
}

/* { dg-error "'-fcf-protection' is not compatible with this target" "" { target *-*-* } 0 } */
