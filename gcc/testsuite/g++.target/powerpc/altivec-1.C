/* { dg-do compile } */
/* { dg-options "-maltivec" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>
 
int main()
{
  return 0;
}

class F32vec4 {
public:
  vector float val;
  vector float operator++(void) { return val;}
};
