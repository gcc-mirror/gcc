/* { dg-do compile } */
/* { dg-options "-O2 -fcf-protection=return -mfunction-return=thunk -mindirect-branch=thunk" } */

void
bar (void)
{
/* { dg-error "'-mindirect-branch' and '-fcf-protection' are not compatible" "" { target *-*-* } .-1 } */
/* { dg-error "'-mfunction-return' and '-fcf-protection' are not compatible" "" { target *-*-* } .-2 } */
}
