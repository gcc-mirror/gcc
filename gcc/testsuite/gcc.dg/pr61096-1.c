/* PR c/61077 */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -fshort-enums -fshort-wchar -Wpedantic" } */
/* { dg-prune-output ".*near initialization for.*" } */

typedef enum { A } schar;
extern int e;
struct S
{
  int a[3];
};
struct f
{
  int w;
  int x[];
};
struct g
{
  struct f f; /* { dg-warning "invalid use of structure with flexible array member" } */
};

char w1[] = L"foo"; /* { dg-error "13:array of .char. from a string literal with type array of" } */
__WCHAR_TYPE__ w2[] = "foo"; /* { dg-error "23:from a string literal with type array of .char." } */
__WCHAR_TYPE__ w3[] = U"foo"; /* { dg-error "23:from a string literal with type array of" } */
schar a1[] = "foo"; /* { dg-error "14:array of inappropriate type initialized from string constant" } */
int a2[] = (int[]) { 1 }; /* { dg-warning "12:initializer element is not constant" } */

int a3 = e; /* { dg-error "10:initializer element is not constant" } */
int a4 = (e, 1); /* { dg-error "10:initializer element is not constant" } */
int a5 = a1[0]; /* { dg-error "10:initializer element is not constant" } */
int a6 = &a3 - &a4; /* { dg-error "10:initializer element is not" } */
int a7[] = a7; /* { dg-error "12:invalid initializer" } */

struct S s = { { 1 }, { 3 } }; /* { dg-error "23:extra brace group at end of initializer" } */
/* { dg-warning "23:excess elements in struct initializer" "" { target *-*-* } .-1 } */
struct g g1 = { {0, { 1 } } }; /* { dg-error "21:initialization of flexible array member in a nested context" } */
struct g g2 = { .f[0] = 1 }; /* { dg-error "20:array index in non-array initializer" } */

__extension__ int a8 = { }; /* { dg-error "24:empty scalar initializer" } */
int a9[10] = {[1.2] = 2 }; /* { dg-error "16:array index in initializer not of integer type" } */
int a10[10] = {[e] = 2 }; /* { dg-error "17:nonconstant array index in initializer" } */
__extension__ int a11[10] = {[1 ... e] = 1 }; /* { dg-error "31:nonconstant array index in initializer" } */
int a12 = {[1] = 2 }; /* { dg-error "13:array index in non-array initializer" } */
int a13[2] = {[-1] = 4 }; /* { dg-error "16:array index in initializer exceeds array bounds" } */
int a14[2] = {[64] = 4 }; /* { dg-error "16:array index in initializer exceeds array bounds" } */
__extension__ int a15[10] = {[2 ... 1] = 4 }; /* { dg-error "31:empty index range in initializer" } */
__extension__ int a16[10] = {[2 ... 100] = 4 }; /* { dg-error "31:array index range in initializer exceeds array bounds" } */
int a17[] = { .B = 1 }; /* { dg-error "15:field name not in record or union initializer" } */
int a18[] = { e }; /* { dg-error "15:initializer element is not constant" } */
char a19[1] = { "x", "x" }; /* { dg-error "22:excess elements in 'char' array initializer" } */

void
bar (void)
{
  struct f f = { 2, "c" }; /* { dg-error "21:non-static initialization of a flexible array member" } */
}

struct
{
  char *v;
} sx[] = { .v = 0 }; /* { dg-error "12:field name not in record or union initializer" } */
