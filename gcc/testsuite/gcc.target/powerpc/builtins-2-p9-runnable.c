/* { dg-do run } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

#include <altivec.h> // vector

void abort (void);

int main() {
  int i;
  vector int vsia;
  vector unsigned int vsir, vsiexpt;
  vector unsigned int vuia, vuir, vuiexpt;
  vector signed long long vslla;
  vector unsigned long long vsllr, vsllexpt;
  vector unsigned long long vulla, vullr, vullexpt;
  vector __int128_t  vs128a;
  vector __uint128_t vs128r, vs128expt;
  vector __uint128_t vu128a, vu128r, vu128expt;  

  /* Returns a vector with each element containing the parity of the low-order
     bit of each of the bytes in that element.  Note results are always
     returned in an unsinged type, per the ABI spec.  */
  vsia = (vector int) {0x10101010, 0x10101011, 0x10101111, 0x10111111};
  vsiexpt = (vector unsigned int){0x0, 0x1, 0x0, 0x1};

  vuia = (vector unsigned int) {0x000010000, 0x00010001,
				0x10100000, 0x000010101};
  vuiexpt = (vector unsigned int){0x1, 0x0, 0x0, 0x1};

  vslla = (vector long long) {0x0000000000010000,  0x0001000100010000};
  vsllexpt = (vector unsigned long long){0x1, 0x1};

  vulla = (vector unsigned long long)   {0x0000000000000001,
					 0x0001000000000001};
  vullexpt = (vector unsigned long long){0x1, 0x0};

  vs128a = (vector __int128_t) {0x0000000000001};
  vs128expt = (vector __uint128_t) {0x1};
  vu128a = (vector __uint128_t) {0x1000000000001};
  vu128expt = (vector __uint128_t) {0x0};

  vsir = vec_parity_lsbb(vsia);
  vuir = vec_parity_lsbb(vuia);
  vsllr = vec_parity_lsbb(vslla);
  vullr = vec_parity_lsbb(vulla);
  vs128r = vec_parity_lsbb(vs128a);
  vu128r = vec_parity_lsbb(vu128a);

  for(i = 0; i< 4; i++) {
    if (vsir[i] != vsiexpt[i])
      abort();

    if (vuir[i] != vuiexpt[i])
      abort();
  }

  for(i = 0; i< 2; i++) {
    if (vsllr[i] != vsllexpt[i])
      abort();

    if (vullr[i] != vullexpt[i])
      abort();
  }

  if (vs128r[0] != vs128expt[0])
    abort();

  if (vu128r[0] != vu128expt[0])
    abort();
}
