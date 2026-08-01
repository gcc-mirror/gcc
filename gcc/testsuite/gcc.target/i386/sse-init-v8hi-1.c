/* { dg-do compile } */
/* { dg-options "-O2 -msse -mno-sse2" } */

typedef short v8hi __attribute__ ((__vector_size__ (16)));

short a, b, c, d, e, f, g, h;

v8hi fa0000000() { return (v8hi){a,0,0,0,0,0,0,0}; }
v8hi f0a000000() { return (v8hi){0,a,0,0,0,0,0,0}; }
v8hi f00a00000() { return (v8hi){0,0,a,0,0,0,0,0}; }
v8hi f000a0000() { return (v8hi){0,0,0,a,0,0,0,0}; }
v8hi f0000a000() { return (v8hi){0,0,0,0,a,0,0,0}; }
v8hi f00000a00() { return (v8hi){0,0,0,0,0,a,0,0}; }
v8hi f000000a0() { return (v8hi){0,0,0,0,0,0,a,0}; }
v8hi f0000000a() { return (v8hi){0,0,0,0,0,0,0,a}; }
v8hi fabcdefgh() { return (v8hi){a,b,c,d,e,f,g,h}; }

/* { dg-final { scan-assembler-times "movaps" 9 } } */
