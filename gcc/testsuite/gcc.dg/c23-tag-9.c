/* { dg-do compile } */
/* { dg-options "-std=c23" } */

struct foo { int x; } x;
struct foo { alignas(128) int x; } y;	/* { dg-error "redefinition" } */
static_assert(alignof(y) == 128);


