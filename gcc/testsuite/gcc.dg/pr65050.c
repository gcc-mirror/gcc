/* PR c/65050 */
/* { dg-do compile } */

typedef int A[];
struct S { int i; A a[5]; } s; /* { dg-error "array type has incomplete element type 'A' {aka 'int\\\[\\\]'}" } */
extern void foo (int p[2][]); /* { dg-error "array type has incomplete element type .int\\\[\\\]." } */
extern void bar (A p[2]); /* { dg-error "array type has incomplete element type 'A' {aka 'int\\\[\\\]'}" } */

void
foo (int p[2][]) /* { dg-error "array type has incomplete element type .int\\\[\\\]." } */
{
}

void
bar (A p[2]) /* { dg-error "array type has incomplete element type 'A' {aka 'int\\\[\\\]'}" } */
{
}

struct T;
struct T t[5]; /* { dg-error "array type has incomplete element type .struct T." } */
struct U u[] = { { "abc" } }; /* { dg-error "array type has incomplete element type .struct U." } */
typedef struct T TT;
TT tt[5]; /* { dg-error "array type has incomplete element type 'TT' {aka 'struct T'}" } */
