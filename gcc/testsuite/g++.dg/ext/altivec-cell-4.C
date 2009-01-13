/* { dg-do run { target { powerpc*-*-* && vmx_hw } } } */
/* { dg-do compile { target { powerpc*-*-* && { ! vmx_hw } } } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

/* Test the vec_splats and vec_promote VMX intrinsics.  */
#include <altivec.h>
    
extern "C" void abort (void);
        
vector int a[] = {{0, 0, 0, 0}, {1,0,0,0}, {1,2,0,0},{1,2,3,0},{1,2,3,4},{5,2,3,4},{5,6,3,4}};
vector int c = {0,6,3,4};
vector int d = {0,0,3,4};
int main1(int t) __attribute__((noinline));
int main1(int t)
{ 
  int i;
  vector int b = vec_splats(0);
  for(i = 0;i<sizeof(a)/sizeof(a[0])-1;i++)
    { 
        if (__builtin_memcmp (&b, &a[i], sizeof(vector int)))
          abort ();
        b = vec_insert(i+1, b, i);
    } 
  if (__builtin_memcmp (&b, &a[i], sizeof(vector int)))
    abort ();
  
  b = vec_insert(0, b, 0);
  if (__builtin_memcmp (&b, &c, sizeof(vector int)))
    abort ();
  
  b = vec_insert(0, b, 1);
  if (__builtin_memcmp (&b, &d, sizeof(vector int)))
    abort ();
  
  return 0;
} 
  
int main(void)
{
  return main1 (0);
}
