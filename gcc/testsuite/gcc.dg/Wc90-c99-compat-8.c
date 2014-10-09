/* { dg-do compile } */
/* { dg-options "-std=gnu90 -pedantic-errors -Wc90-c99-compat" } */

_Bool b; /* { dg-error "ISO C90 does not support boolean types" } */
_Complex double c = __builtin_complex (0.0, 0.0); /* { dg-error "ISO C90 does not support complex types" } */
long long l; /* { dg-error "ISO C90 does not support .long long." } */
struct A { int i; char a[]; }; /* { dg-error "ISO C90 does not support flexible array members" } */
struct { long int b: 2; } s; /* { dg-error "type of bit-field .b. is a GCC extension" } */
const const int i; /* { dg-error "duplicate .const." } */
volatile volatile v; /* { dg-error "duplicate .volatile." } */

struct S { int a[2]; };
extern struct S foo (void);

#define V(v, ...) (v, __VA_ARGS) /* { dg-error "anonymous variadic macros were introduced in C99" } */

enum { E, }; /* { dg-error "comma at end of enumerator list" } */

void fn1 (char [*]); /* { dg-error "ISO C90 does not support .\\\[\\\*\\\]. array declarators" } */

void
fn2 (char x[static 4]) /* { dg-error "ISO C90 does not support .static. or type qualifiers" } */
{
  int i = (int) { 1 }; /* { dg-error "ISO C90 forbids compound literals" } */
  struct A a = { .i = 3 }; /* { dg-error "ISO C90 forbids specifying subobject to initialize" } */
}

void
fn3 (int n)
{
  n = 3;
  int i; /* { dg-error "ISO C90 forbids mixed declarations and code" } */
}

void
fn4 (int n)
{
  n = 3;
  __extension__ int i; /* { dg-error "ISO C90 forbids mixed declarations and code" } */
}

void
fn5 (void)
{
  (foo ()).a[0]; /* { dg-error "ISO C90 forbids subscripting non-lvalue array" } */
}

#define F(a) a

void
fn6 (void)
{
  F(); /* { dg-error "invoking macro F argument" } */
}

void fn7 (int n, int a[n]); /* { dg-error "ISO C90 forbids variable length array .a." } */
