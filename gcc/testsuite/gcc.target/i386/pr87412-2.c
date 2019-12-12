/* { dg-do compile } */
/* { dg-options "-O2 -fcf-protection -mfunction-return=keep -mindirect-branch=keep" } */

void
 __attribute__ ((indirect_branch("thunk"), function_return("thunk")))
bar (void)
{
/* { dg-error "'-mindirect-branch' and '-fcf-protection' are not compatible" "" { target *-*-* } .-1 } */
/* { dg-error "'-mfunction-return' and '-fcf-protection' are not compatible" "" { target *-*-* } .-2 } */
}
