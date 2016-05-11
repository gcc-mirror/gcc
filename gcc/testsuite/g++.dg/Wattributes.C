// PR c++/38611
// { dg-do compile }

struct A { unsigned char a [272]; };

typedef struct __attribute__ ((aligned (128))) A B;   // { dg-warning "attributes ignored" }
typedef struct __attribute__ ((__may_alias__)) A C;   // { dg-warning "attributes ignored" }

#ifndef __cplusplus
#  define alignof       _Alignof
#  define static_assert _Static_assert
#elif __cplusplus < 201103L
#  define alignof __alignof__
#  define static_assert(expr, ignore) typedef int Assert [(expr) ? 1 : -1]
#endif

#define SA(expr) static_assert ((expr), #expr)

SA (alignof (struct A) == alignof (B));

