/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-O2 -mindirect-branch=thunk -fcf-protection -mcet -fcheck-pointer-bounds -mmpx" } */

void
bar (void)
{
}
