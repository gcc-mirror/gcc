/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-O2 -mindirect-branch=thunk -fcf-protection -fcheck-pointer-bounds -mmpx" } */

void
bar (void)
{
}
