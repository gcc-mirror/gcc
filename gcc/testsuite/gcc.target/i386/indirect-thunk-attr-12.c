/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-O2 -mindirect-branch=keep -fcf-protection -fcheck-pointer-bounds -mmpx" } */

__attribute__ ((indirect_branch("thunk-extern")))
void
bar (void)
{ /* { dg-error "'-mindirect-branch=thunk-extern', '-fcf-protection=branch' and '-fcheck-pointer-bounds' are not compatible" } */
}
