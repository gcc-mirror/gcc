/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

#define f0() void x0 (vector float x) { }
f0 ()

#define f1(type) void x1##type (vector type x) { }
f1 (float)

#define f2(v, type) void x2##type (v type x) { }
f2 (vector, float)

#define f3(type) void x3##type (vector bool type x) { }
f3 (int)

#define f4(v, type) void x4##type (v bool type x) { }
f4 (vector, int)

#define f5(b, type) void x5##type (vector b type x) { }
f5 (bool, int)

#define f6(v, b, type) void x6##type (v b type x) { }
f6 (vector, bool, int)

#define f7(v, b, type) void x7##type (v type b x) { }
f7 (vector, bool, int)

int vector = 6;

#define v1(v) int x8 (int v) { return v; }
v1(vector)
