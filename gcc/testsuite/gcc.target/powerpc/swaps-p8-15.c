/* { dg-do compile { target { powerpc64le-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O3" } */
/* { dg-final { scan-assembler "lxvd2x" } } */
/* { dg-final { scan-assembler "stxvd2x" } } */
/* { dg-final { scan-assembler "xxspltw" } } */

/* Currently the analyze_swaps phase cannot optimize this loop because
   of the presence of an UNSPEC_VSX_CVDPSPN.  At such time as this is 
   handled, we need to add a 'scan-assembler-not "xxpermdi"' directive to
   this test.  */
#include <altivec.h>
void abort();

#define N 4096
#define M 10000000
vector float ca[N][4] = {0};
vector float cb[N][4] = {0};
vector float cc[N][4] = {0};

__attribute__((noinline)) void foo ()
{
  int i;
  for (i = 0; i < N; i++) {
    cc[i][0] = vec_mul(vec_splats(cb[i][0][0]), ca[i][0]);
    cc[i][0] = vec_madd(cc[i][0],vec_splats(cb[i][0][1]), ca[i][1]);
    cc[i][0] = vec_madd(cc[i][0],vec_splats(cb[i][0][2]), ca[i][2]);
    cc[i][0] = vec_madd(cc[i][0],vec_splats(cb[i][0][3]), ca[i][3]);

    cc[i][1] = vec_mul(vec_splats(cb[i][1][0]), ca[i][0]);
    cc[i][1] = vec_madd(cc[i][0],vec_splats(cb[i][1][1]), ca[i][1]);
    cc[i][1] = vec_madd(cc[i][0],vec_splats(cb[i][1][2]), ca[i][2]);
    cc[i][1] = vec_madd(cc[i][0],vec_splats(cb[i][1][3]), ca[i][3]);
    
    cc[i][2] = vec_mul(vec_splats(cb[i][2][0]), ca[i][0]);
    cc[i][2] = vec_madd(cc[i][0],vec_splats(cb[i][2][1]), ca[i][1]);
    cc[i][2] = vec_madd(cc[i][0],vec_splats(cb[i][2][2]), ca[i][2]);
    cc[i][2] = vec_madd(cc[i][0],vec_splats(cb[i][2][3]), ca[i][3]);
    
    cc[i][3] = vec_mul(vec_splats(cb[i][3][0]), ca[i][0]);
    cc[i][3] = vec_madd(cc[i][0],vec_splats(cb[i][3][1]), ca[i][1]);
    cc[i][3] = vec_madd(cc[i][0],vec_splats(cb[i][3][2]), ca[i][2]);
    cc[i][3] = vec_madd(cc[i][0],vec_splats(cb[i][3][3]), ca[i][3]);
  }
}

int main ()
{
  foo ();
  return 0;
}
