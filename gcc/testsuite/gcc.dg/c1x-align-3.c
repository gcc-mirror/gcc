/* Test C1X alignment support.  Test invalid code.  */
/* { dg-do compile } */
/* { dg-options "-std=c1x -pedantic-errors" } */

int a = _Alignof (void (void)); /* { dg-error "function" } */
struct s;
int b = _Alignof (struct s); /* { dg-error "incomplete" } */
int c = _Alignof (void); /* { dg-error "void" } */
int d = _Alignof (a); /* { dg-error "expression" } */

_Alignas (void (void)) char e; /* { dg-error "function" } */
_Alignas (struct s) char f; /* { dg-error "incomplete" } */
_Alignas (void) char g; /* { dg-error "void" } */

_Alignas (-__INT_MAX__-1) char h; /* { dg-error "too large|power of 2" } */
_Alignas (-__INT_MAX__) char h2; /* { dg-error "too large|power of 2" } */
_Alignas ((-__INT_MAX__-1)/2) char h3; /* { dg-error "too large|power of 2" } */
_Alignas ((-__INT_MAX__-1)/4) char h4; /* { dg-error "too large|power of 2" } */
_Alignas ((-__INT_MAX__-1)/8) char h5; /* { dg-error "too large|power of 2" } */
_Alignas (-__LONG_LONG_MAX__-1) char i; /* { dg-error "too large|power of 2" } */
_Alignas (-(__LONG_LONG_MAX__-1)/2) char i2; /* { dg-error "too large|power of 2" } */
_Alignas (-(__LONG_LONG_MAX__-1)/4) char i3; /* { dg-error "too large|power of 2" } */
_Alignas (-(__LONG_LONG_MAX__-1)/8) char i4; /* { dg-error "too large|power of 2" } */
_Alignas (-(__LONG_LONG_MAX__-1)/16) char i5; /* { dg-error "too large|power of 2" } */
_Alignas (-1) char j; /* { dg-error "power of 2" } */
_Alignas (-2) char j; /* { dg-error "positive power of 2" } */
_Alignas (3) char k; /* { dg-error "power of 2" } */

_Alignas ((void *) 1) char k; /* { dg-error "integer constant" } */
int x;
_Alignas (x) char l; /* { dg-error "integer constant" } */

_Alignas (0) struct s; /* { dg-error "does not redeclare tag" } */

_Alignas (0) typedef int T; /* { dg-error "alignment specified for typedef" } */
void func (_Alignas (0) int); /* { dg-error "alignment specified for unnamed parameter" } */
void f2 (_Alignas (0) int parm2) {} /* { dg-error "alignment specified for parameter" } */
void
f3 (void)
{
  register _Alignas (0) int reg; /* { dg-error "register" } */
}
_Alignas (0) void f4 (void); /* { dg-error "alignment specified for function" } */
