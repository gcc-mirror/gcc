/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-O2 -mindirect-branch=thunk-extern -fcf-protection -fcheck-pointer-bounds -mmpx" } */

void
bar (void)
{ /* { dg-error "'-mindirect-branch=thunk-extern', '-fcf-protection=branch' and '-fcheck-pointer-bounds' are not compatible" } */
}
