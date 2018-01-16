/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-O2 -mfunction-return=thunk-extern -mindirect-branch=keep -mcmodel=large" } */
/* { dg-additional-options "-fPIC" { target fpic } } */

void
bar (void)
{ /* { dg-error "'-mfunction-return=thunk-extern' and '-mcmodel=large' are not compatible" } */
}
