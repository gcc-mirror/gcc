/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -msse2 -mno-sse4.1" } */

typedef char v16qi __attribute__ ((__vector_size__ (16)));

char a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p;

v16qi fa000000000000000() { return (v16qi){a,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}; }
v16qi f0a00000000000000() { return (v16qi){0,a,0,0,0,0,0,0,0,0,0,0,0,0,0,0}; }
v16qi f00a0000000000000() { return (v16qi){0,0,a,0,0,0,0,0,0,0,0,0,0,0,0,0}; }
v16qi f000a000000000000() { return (v16qi){0,0,0,a,0,0,0,0,0,0,0,0,0,0,0,0}; }
v16qi f0000a00000000000() { return (v16qi){0,0,0,0,a,0,0,0,0,0,0,0,0,0,0,0}; }
v16qi f00000a0000000000() { return (v16qi){0,0,0,0,0,a,0,0,0,0,0,0,0,0,0,0}; }
v16qi f000000a000000000() { return (v16qi){0,0,0,0,0,0,a,0,0,0,0,0,0,0,0,0}; }
v16qi f0000000a00000000() { return (v16qi){0,0,0,0,0,0,0,a,0,0,0,0,0,0,0,0}; }
v16qi f00000000a0000000() { return (v16qi){0,0,0,0,0,0,0,0,a,0,0,0,0,0,0,0}; }
v16qi f000000000a000000() { return (v16qi){0,0,0,0,0,0,0,0,0,a,0,0,0,0,0,0}; }
v16qi f0000000000a00000() { return (v16qi){0,0,0,0,0,0,0,0,0,0,a,0,0,0,0,0}; }
v16qi f00000000000a0000() { return (v16qi){0,0,0,0,0,0,0,0,0,0,0,a,0,0,0,0}; }
v16qi f000000000000a000() { return (v16qi){0,0,0,0,0,0,0,0,0,0,0,0,a,0,0,0}; }
v16qi f0000000000000a00() { return (v16qi){0,0,0,0,0,0,0,0,0,0,0,0,0,a,0,0}; }
v16qi f00000000000000a0() { return (v16qi){0,0,0,0,0,0,0,0,0,0,0,0,0,0,a,0}; }
v16qi f000000000000000a() { return (v16qi){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,a}; }

v16qi faa00000000000000() { return (v16qi){a,a,0,0,0,0,0,0,0,0,0,0,0,0,0,0}; }
v16qi fa0a0000000000000() { return (v16qi){a,0,a,0,0,0,0,0,0,0,0,0,0,0,0,0}; }
v16qi fa000a00000000000() { return (v16qi){a,0,0,0,a,0,0,0,0,0,0,0,0,0,0,0}; }
v16qi fa0000000a0000000() { return (v16qi){a,0,0,0,0,0,0,0,a,0,0,0,0,0,0,0}; }

v16qi faaaaaaaaaaaaaaaa() { return (v16qi){a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a}; }
v16qi faaaaaaaabbbbbbbb() { return (v16qi){a,a,a,a,a,a,a,a,b,b,b,b,b,b,b,b}; }
v16qi faaaabbbbaaaabbbb() { return (v16qi){a,a,a,a,b,b,b,b,a,a,a,a,b,b,b,b}; }
v16qi fabababababababab() { return (v16qi){a,b,a,b,a,b,a,b,a,b,a,b,a,b,a,b}; }

v16qi fabcd000000000000() { return (v16qi){a,b,c,d,0,0,0,0,0,0,0,0,0,0,0,0}; }
v16qi fabcdefgh00000000() { return (v16qi){a,b,c,d,e,f,g,h,0,0,0,0,0,0,0,0}; }
v16qi fabcdefghijklmnop() { return (v16qi){a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p}; }

/* { dg-final { scan-assembler-times "addq" 42 } } */
/* { dg-final { scan-assembler-not "movaps" } } */
/* { dg-final { scan-assembler-times "movd" 17 } } */
/* { dg-final { scan-assembler-times "movq" 25 } } */
/* { dg-final { scan-assembler-times "movzbl" 55 } } */
/* { dg-final { scan-assembler-times "pshufd" 1 } } */
/* { dg-final { scan-assembler-times "pslldq" 12 } } */
/* { dg-final { scan-assembler-times "punpcklbw" 1 } } */
/* { dg-final { scan-assembler-times "punpcklwd" 1 } } */
/* { dg-final { scan-assembler-times "punpcklqdq" 5 } } */
/* { dg-final { scan-assembler-times "sall" 3 } } */
/* { dg-final { scan-assembler-times "salq" 42 } } */
