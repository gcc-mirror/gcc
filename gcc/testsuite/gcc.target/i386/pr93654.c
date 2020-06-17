/* { dg-do compile } */
/* { dg-options "-O2 -fcf-protection -mfunction-return=thunk-extern -mindirect-branch=thunk-extern" } */

int
bar (void (*foo) (void))
{
  foo (); 
  return 0;
}
