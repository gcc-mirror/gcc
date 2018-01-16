/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-O2 -mfunction-return=thunk -mindirect-branch=keep -mcmodel=large" } */

void
bar (void)
{ /* { dg-error "'-mfunction-return=thunk' and '-mcmodel=large' are not compatible" } */
}
