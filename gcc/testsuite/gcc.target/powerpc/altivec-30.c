/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

#include <stdbool.h>
#include <altivec.h>

#define f0(type) void x0##type (vector bool type x) { }
f0 (int)

#define f1(v, type) void x1##type (v bool type x) { }
f1 (vector, int)

#define f2(b, type) void x2##type (vector b type x) { }
f2 (bool, int)

#define f3(v, b, type) void x3##type (v b type x) { }
f3 (vector, bool, int)

#define f4(v, b, type) void x4##type (v type b x) { }
f4 (vector, bool, int)

#define B bool
#define I int
#define BI bool int
#define VBI vector bool int

vector bool int a;
vector B int b;
vector B I c;
vector BI d;
VBI e;
