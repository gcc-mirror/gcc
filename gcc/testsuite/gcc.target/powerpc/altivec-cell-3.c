/* { dg-do run { target { powerpc*-*-* && vmx_hw } } } */
/* { dg-do compile { target { powerpc*-*-* && { ! vmx_hw } } } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */
/* Test the vec_splats and vec_promote VMX intrinsics.  */
#include <altivec.h>
    
extern void abort (void);
        
vector int a = {0, 0, 0, 0};
int main1(int t) __attribute__((noinline));
int main1(int t)
{ 
  int i;
  vector int b = vec_splats(0);
  if (__builtin_memcmp (&a, &b, sizeof(vector int)))
    abort ();
  
  b = vec_splats(t);
  if (__builtin_memcmp (&a, &b, sizeof(vector int)))
    abort ();
  
  b = vec_promote(0, 1);
  if (vec_extract (b, 1) != 0)
    abort ();
  
  b = vec_promote(t, t);
  if (vec_extract (b, t) != 0)
    abort ();
  
  return 0;
} 
    
int main(void)
{ 
  return main1 (0);
}
