/* PR target/65146 */
/* { dg-do compile } */
/* { dg-options "-Wno-psabi" } */

struct A { char a; _Atomic long long b; };
struct B { char a; _Atomic double b; };
struct C { char a; _Atomic long long b[2]; };
struct D { char a; _Atomic double b[2]; };
extern int a[__builtin_offsetof (struct A, b) == 8 ? 1 : -1];
extern int b[__builtin_offsetof (struct B, b) == 8 ? 1 : -1];
extern int c[__builtin_offsetof (struct C, b) == 8 ? 1 : -1];
extern int d[__builtin_offsetof (struct D, b) == 8 ? 1 : -1];
