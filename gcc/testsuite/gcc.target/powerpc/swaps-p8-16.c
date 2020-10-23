/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O3" } */
/* { dg-final { scan-assembler "lxvd2x" } } */
/* { dg-final { scan-assembler "stxvd2x" } } */
/* { dg-final { scan-assembler "vspltw" } } */
/* { dg-final { scan-assembler-not "xxpermdi" } } */

#include <altivec.h>
void abort();

typedef struct xx {vector double l; vector double h;} xx;

#define N 4096
#define M 10000000
vector float ca[N][4] = {0};
vector float cb[N][4] = {0};
vector float cc[N][4] = {0};

__attribute__((noinline)) void foo ()
{
  int i;
  vector float brow;

  for (i = 0; i < N; i++) {

    brow = cb[i][0];
    cc[i][0] = vec_mul(vec_splats(brow[0]), ca[i][0]);
    cc[i][0] = vec_madd(cc[i][0],vec_splats(brow[1]), ca[i][1]);
    cc[i][0] = vec_madd(cc[i][0],vec_splats(brow[2]), ca[i][2]);
    cc[i][0] = vec_madd(cc[i][0],vec_splats(brow[3]), ca[i][3]);

    brow = cb[i][1];
    cc[i][1] = vec_mul(vec_splats(brow[0]), ca[i][0]);
    cc[i][1] = vec_madd(cc[i][0],vec_splats(brow[1]), ca[i][1]);
    cc[i][1] = vec_madd(cc[i][0],vec_splats(brow[2]), ca[i][2]);
    cc[i][1] = vec_madd(cc[i][0],vec_splats(brow[3]), ca[i][3]);
    
    brow = cb[i][2];
    cc[i][2] = vec_mul(vec_splats(brow[0]), ca[i][0]);
    cc[i][2] = vec_madd(cc[i][0],vec_splats(brow[1]), ca[i][1]);
    cc[i][2] = vec_madd(cc[i][0],vec_splats(brow[2]), ca[i][2]);
    cc[i][2] = vec_madd(cc[i][0],vec_splats(brow[3]), ca[i][3]);
    
    brow = cb[i][3];
    cc[i][3] = vec_mul(vec_splats(brow[0]), ca[i][0]);
    cc[i][3] = vec_madd(cc[i][0],vec_splats(brow[1]), ca[i][1]);
    cc[i][3] = vec_madd(cc[i][0],vec_splats(brow[2]), ca[i][2]);
    cc[i][3] = vec_madd(cc[i][0],vec_splats(brow[3]), ca[i][3]);
  }
}

int main ()
{
  foo ();
  return 0;
}
