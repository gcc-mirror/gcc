/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wc90-c99-compat -pedantic-errors" } */

_Bool b; /* { dg-warning "ISO C90 does not support boolean types" } */
_Complex double c = __builtin_complex (0.0, 0.0); /* { dg-warning "ISO C90 does not support complex types" } */
long long l; /* { dg-warning "ISO C90 does not support .long long." } */
struct A { int i; char a[]; }; /* { dg-warning "ISO C90 does not support flexible array members" } */
struct { long int b: 2; } s; /* { dg-warning "type of bit-field .b. is a GCC extension" } */
const const int i; /* { dg-warning "duplicate .const." } */
volatile volatile int v; /* { dg-warning "duplicate .volatile." } */

struct S { int a[2]; };
extern struct S foo (void);

#define V(v, ...) (v, __VA_ARGS) /* { dg-warning "anonymous variadic macros were introduced in C99" } */

enum { E, }; /* { dg-warning "comma at end of enumerator list" } */

void fn1 (char [*]); /* { dg-warning "ISO C90 does not support .\\\[\\\*\\\]. array declarators" } */

void
fn2 (char x[static 4]) /* { dg-warning "ISO C90 does not support .static. or type qualifiers" } */
{
  int i = (int) { 1 }; /* { dg-warning "ISO C90 forbids compound literals" } */
  struct A a = { .i = 3 }; /* { dg-warning "ISO C90 forbids specifying subobject to initialize" } */
}

void
fn3 (int n)
{
  n = 3;
  int i; /* { dg-warning "ISO C90 forbids mixed declarations and code" } */
}

void
fn4 (int n)
{
  n = 3;
  __extension__ int i; /* { dg-warning "ISO C90 forbids mixed declarations and code" } */
}

void
fn5 (void)
{
  (foo ()).a[0]; /* { dg-warning "ISO C90 forbids subscripting non-lvalue array" } */
}

#define F(a) a

void
fn6 (void)
{
  F(); /* { dg-warning "invoking macro F argument" } */
}

void fn7 (int n, int a[n]); /* { dg-warning "ISO C90 forbids variable length array .a." } */
