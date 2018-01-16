/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-O2 -mindirect-branch=thunk-extern -mfunction-return=keep -mcmodel=large" } */

void
bar (void)
{ /* { dg-error "'-mindirect-branch=thunk-extern' and '-mcmodel=large' are not compatible" } */
}
