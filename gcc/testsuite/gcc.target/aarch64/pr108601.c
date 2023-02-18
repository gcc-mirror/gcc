/* { dg-do compile } */
/* { dg-options "-O3 -fprofile-generate -mcpu=neoverse-v1" } */

int
foo() {
  int flag = 1;
  for (; flag <= 1 << 21; flag <<= 1)
    ;
  return 0;
}

