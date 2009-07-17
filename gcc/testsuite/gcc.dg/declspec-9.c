/* Test declaration specifiers.  Test various checks on storage class
   and function specifiers that depend on information about the
   declaration, not just the specifiers.  Test with no special
   options.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

auto void f0 (void) {} /* { dg-warning "function definition declared 'auto'" } */
register void f1 (void) {} /* { dg-error "function definition declared 'register'" } */
typedef void f2 (void) {} /* { dg-error "function definition declared 'typedef'" } */

void f3 (auto int); /* { dg-error "storage class specified for unnamed parameter" } */
void f4 (extern int); /* { dg-error "storage class specified for unnamed parameter" } */
void f5 (register int);
void f6 (static int); /* { dg-error "storage class specified for unnamed parameter" } */
void f7 (typedef int); /* { dg-error "storage class specified for unnamed parameter" } */

auto int x; /* { dg-error "file-scope declaration of 'x' specifies 'auto'" } */
register int y; /* { dg-error "register name not specified for 'y'" } */

void h (void) { extern void x (void) {} } /* { dg-error "nested function 'x' declared 'extern'" } */

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

void i (void) { auto void y (void) {} }

inline int main (void) { return 0; } /* { dg-warning "cannot inline function 'main'" } */
