/* { dg-options "-O3" } */

/* This ICEd with  -O3 -mfpu=neon -mfloat-abi=hard -march=armv7-a  .  */

char fn1() {
  long long b[5];
  for (int a = 0; a < 5; a++)
    b[a] = ~0ULL;
  return b[3];
}
