/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl" } */

typedef int v4si __attribute__ ((__vector_size__ (16)));

int m,n;

v4si fa000(int a) { return (v4si){a,0,0,0}; }
v4si f0a00(int a) { return (v4si){0,a,0,0}; }
v4si f00a0(int a) { return (v4si){0,0,a,0}; }
v4si f000a(int a) { return (v4si){0,0,0,a}; }

v4si faa00(int a) { return (v4si){a,a,0,0}; }
v4si fa0a0(int a) { return (v4si){a,0,a,0}; }

v4si faaaa(int a) { return (v4si){a,a,a,a}; }

v4si fab00(int a, int b) { return (v4si){a,b,0,0}; }
v4si fa0b0(int a, int b) { return (v4si){a,0,b,0}; }
v4si fabab(int a, int b) { return (v4si){a,b,a,b}; }
v4si fabcd(int a, int b, int c, int d) { return (v4si){a,b,c,d}; }

v4si fm000() { return (v4si){m,0,0,0}; }
v4si fm0m0() { return (v4si){m,0,m,0}; }
v4si fmmmm() { return (v4si){m,m,m,m}; }

/* { dg-final { scan-assembler-times "vmovd" 12 } } */
/* { dg-final { scan-assembler-times "vpslldq" 3 } } */
/* { dg-final { scan-assembler-times "vshufps" 4 } } */
/* { dg-final { scan-assembler-times "vpbroadcastd" 2 } } */
/* { dg-final { scan-assembler-times "vpinsrd" 6 } } */

