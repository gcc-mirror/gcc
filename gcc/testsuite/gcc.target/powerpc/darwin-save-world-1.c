/* { dg-do run { target powerpc*-*-* } } */
/* { dg-options "-maltivec" } */
/* { dg-require-effective-target powerpc_altivec } */
/* { dg-skip-if "need to be able to execute AltiVec" { ! vmx_hw } } */

/* With altivec turned on, Darwin wants to save the world but we did not mark lr as being saved any more
   as saving the lr is not needed for saving altivec registers.  */

int main (void)
{
  __label__ l1;
  void __attribute__((used)) q(void)
  {
    goto l1;
  }

  l1:;
  return 0;
}
