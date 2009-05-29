/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

#define f0(type) void x0##type (vector _Bool type x) { }
f0 (int)

#define f1(v, type) void x1##type (v _Bool type x) { }
f1 (vector, int)

#define f2(b, type) void x2##type (vector b type x) { }
f2 (_Bool, int)

#define f3(v, b, type) void x3##type (v b type x) { }
f3 (vector, _Bool, int)

#define f4(v, b, type) void x4##type (v type b x) { }
f4 (vector, _Bool, int)

#define B _Bool
#define I int
#define BI _Bool int
#define VBI vector _Bool int

vector _Bool int a;
vector B int b;
vector B I c;
vector BI d;
VBI e;
