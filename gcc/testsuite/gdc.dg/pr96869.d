// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=96869
// { dg-do compile }

__vector(float[0]) var01;
// { dg-error "0 byte vector type __vector\\\(float\\\[0\\\]\\\) is not supported on this platform" "" { target *-*-* } .-1 }
__vector(float[3]) var02;
// { dg-error "12 byte vector type __vector\\\(float\\\[3\\\]\\\) is not supported on this platform" "" { target *-*-* } .-1 }
__vector(float[][4]) var03;
// { dg-error "vector type __vector\\\(float\\\[\\\]\\\[4\\\]\\\) is not supported on this platform" "" { target *-*-* } .-1 }
__vector(float[4][4]) var04;
// { dg-error "vector type __vector\\\(float\\\[4\\\]\\\[4\\\]\\\) is not supported on this platform" "" { target *-*-* } .-1 }
__vector(float[float][4]) var05;
// { dg-error "vector type __vector\\\(float\\\[float\\\]\\\[4\\\]\\\) is not supported on this platform" "" { target *-*-* } .-1 }
__vector(float function()[4]) var06;
// { dg-error "vector type __vector\\\(float function\\\(\\\)\\\[4\\\]\\\) is not supported on this platform" "" { target *-*-* } .-1 }
__vector(float delegate()[4]) var07;
// { dg-error "vector type __vector\\\(float delegate\\\(\\\)\\\[4\\\]\\\) is not supported on this platform" "" { target *-*-* } .-1 }
enum E { a, b, c }
__vector(E[4]) var08;
// { dg-error "vector type __vector\\\(E\\\[4\\\]\\\) is not supported on this platform" "" { target *-*-* } .-1 }
struct S { float a; }
__vector(S[4]) var09;
// { dg-error "vector type __vector\\\(S\\\[4\\\]\\\) is not supported on this platform" "" { target *-*-* } .-1 }
class C { float a; }
__vector(C[4]) var10;
// { dg-error "vector type __vector\\\(C\\\[4\\\]\\\) is not supported on this platform" "" { target *-*-* } .-1 }
__vector(cfloat[4]) var11;
// { dg-error "vector type __vector\\\(cfloat\\\[4\\\]\\\) is not supported on this platform" "" { target *-*-* } .-1 }
__vector(bool[4]) var12;
// { dg-error "vector type __vector\\\(bool\\\[4\\\]\\\) is not supported on this platform" "" { target *-*-* } .-1 }
__vector(real[128]) var13;
// { dg-error "vector type __vector\\\(real\\\[128\\\]\\\) is not supported on this platform" "" { target *-*-* } .-1 }
