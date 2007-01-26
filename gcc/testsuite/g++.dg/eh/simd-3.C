// { dg-options "-O" }
// { dg-options "-O -maltivec" { target { powerpc*-*-darwin* && powerpc_altivec_ok } } }
// { dg-do run }

#include <cstdlib>
#include <cstring>

typedef int __attribute__((vector_size(16))) v;

v vv[32];
volatile v vt = { 1, 2, 3, 4 };

void clobber_vrs(void) { };

void (*volatile fp)() = clobber_vrs;

void thrower(void)
{
  v v00 = vv[ 0];
  v v01 = vv[ 1];
  v v02 = vv[ 2];
  v v03 = vv[ 3];
  v v04 = vv[ 4];
  v v05 = vv[ 5];
  v v06 = vv[ 6];
  v v07 = vv[ 7];
  v v08 = vv[ 8];
  v v09 = vv[ 9];
  v v10 = vv[10];
  v v11 = vv[11];
  v v12 = vv[12];

  fp();

  vv[ 0] = v00;
  vv[ 1] = v01;
  vv[ 2] = v02;
  vv[ 3] = v03;
  vv[ 4] = v04;
  vv[ 5] = v05;
  vv[ 6] = v06;
  vv[ 7] = v07;
  vv[ 8] = v08;
  vv[ 9] = v09;
  vv[10] = v10;
  vv[11] = v11;
  vv[12] = v12;

  throw 3;
}

v v2;

int main(void)
{
  v v1 = vt;
  try {
    thrower();
  } catch (int x) {
  }
  v2 = v1;
  if (memcmp (&v2, (v *)&vt, sizeof (v2)) != 0)
    abort ();
  return 0;
}
