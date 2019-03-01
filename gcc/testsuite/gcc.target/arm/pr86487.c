/* { dg-skip-if "" { *-*-* } { "-march=armv[0-6]*" "-mthumb" } { "" } } */
/* { dg-require-effective-target arm_neon_hw } */
/* { dg-options "-O1 -mbig-endian" } */
/* { dg-add-options arm_neon } */
int a, b, c, d;
long long fn1(long long p2) { return p2 == 0 ? -1 : -1 % p2; }
void fn2(long long p1, short p2, long p3) {
  b = fn1((d || 6) & a);
  c = b | p3;
}
