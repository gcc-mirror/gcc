/* Test that imaginary constants are accepted in C2Y mode: compat warnings.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -Wc23-c2y-compat" } */
/* { dg-add-options float32 } */
/* { dg-add-options float64 } */
/* { dg-add-options float32x } */
/* { dg-require-effective-target float32 } */
/* { dg-require-effective-target float32x } */
/* { dg-require-effective-target float64 } */

_Complex _Float32 a = 1.if32;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex _Float32 b = 2.F32j;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex _Float32 c = 3.f32i;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex _Float32 d = 4.JF32;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex _Float64 e = 1.if64;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex _Float64 f = 2.F64j;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex _Float64 g = 3.f64i;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex _Float64 h = 4.JF64;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex _Float32x i = 1.if32x;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex _Float32x j = 2.F32xj;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex _Float32x k = 3.f32xI;	/* { dg-warning "imaginary constants are a C2Y feature" } */
_Complex _Float32x l = 4.JF32x;	/* { dg-warning "imaginary constants are a C2Y feature" } */
__extension__ _Complex _Float32 m = 1.if32;
__extension__ _Complex _Float32 n = 2.F32j;
__extension__ _Complex _Float32 o = 3.f32i;
__extension__ _Complex _Float32 p = 4.JF32;
__extension__ _Complex _Float64 q = 1.if64;
__extension__ _Complex _Float64 r = 2.F64j;
__extension__ _Complex _Float64 s = 3.f64i;
__extension__ _Complex _Float64 t = 4.JF64;
__extension__ _Complex _Float32x u = 1.if32x;
__extension__ _Complex _Float32x v = 2.F32xj;
__extension__ _Complex _Float32x w = 3.f32xI;
__extension__ _Complex _Float32x x = 4.JF32x;
