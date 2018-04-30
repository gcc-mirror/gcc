/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-O2 -mindirect-branch=keep -fcf-protection -fcheck-pointer-bounds -mmpx" } */

__attribute__ ((indirect_branch("thunk-inline")))
void
bar (void)
{
}
