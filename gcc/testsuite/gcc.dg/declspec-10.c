/* Test declaration specifiers.  Test various checks on storage class
   and function specifiers that depend on information about the
   declaration, not just the specifiers.  Test with -pedantic.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-pedantic" } */

auto void f0 (void) {} /* { dg-warning "function definition declared 'auto'" } */
register void f1 (void) {} /* { dg-error "function definition declared 'register'" } */
typedef void f2 (void) {} /* { dg-error "function definition declared 'typedef'" } */

void f3 (auto int); /* { dg-error "storage class specified for parameter 'type name'" } */
void f4 (extern int); /* { dg-error "storage class specified for parameter 'type name'" } */
void f5 (register int);
void f6 (static int); /* { dg-error "storage class specified for parameter 'type name'" } */
void f7 (typedef int); /* { dg-error "storage class specified for parameter 'type name'" } */

auto int x; /* { dg-error "file-scope declaration of 'x' specifies 'auto'" } */
register int y; /* { dg-warning "file-scope declaration of 'y' specifies 'register'" } */

void h (void) { extern void x (void) {} } /* { dg-error "nested function 'x' declared 'extern'" } */
/* { dg-warning "ISO C forbids nested functions" "nested" { target *-*-* } 21 } */

void
g (void)
{
  void a; /* { dg-error "variable or field 'a' declared void" } */
  const void b; /* { dg-error "variable or field 'b' declared void" } */
  static void c; /* { dg-error "variable or field 'c' declared void" } */
}

void p;
const void p1;
extern void q;
extern const void q1;
static void r; /* { dg-error "variable or field 'r' declared void" } */
static const void r1; /* { dg-error "variable or field 'r1' declared void" } */

register void f8 (void); /* { dg-error "invalid storage class for function 'f8'" } */
/* { dg-warning "file-scope declaration of 'f8' specifies 'register'" "register function" { target *-*-* } 39 } */

void i (void) { auto void y (void) {} } /* { dg-warning "ISO C forbids nested functions" } */
/* { dg-warning "function definition declared 'auto'" "nested" { target *-*-* } 42 } */

inline int main (void) { return 0; } /* { dg-warning "cannot inline function 'main'" } */
