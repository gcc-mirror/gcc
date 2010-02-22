/* Test messages for wrong number of arguments to function.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

void f0(void); /* { dg-message "note: declared here" } */
void f1(int); /* { dg-message "note: declared here" } */
void f1v(int, ...); /* { dg-message "note: declared here" } */
void f2(int, int); /* { dg-message "note: declared here" } */
void f2v(int, int, ...); /* { dg-message "note: declared here" } */

struct s {
  void (*f0)(void);
  void (*f1)(int);
  void (*f1v)(int, ...);
  void (*f2)(int, int);
  void (*f2v)(int, int, ...);
} x;

void
g (int a)
{
  f0();
  x.f0();
  f0(a); /* { dg-error "too many arguments to function" } */
  x.f0(a); /* { dg-error "too many arguments to function" } */
  f0(a, a); /* { dg-error "too many arguments to function" } */
  x.f0(a, a); /* { dg-error "too many arguments to function" } */
  f1(); /* { dg-error "too few arguments to function" } */
  x.f1(); /* { dg-error "too few arguments to function" } */
  f1(a);
  x.f1(a);
  f1(a, a); /* { dg-error "too many arguments to function" } */
  x.f1(a, a); /* { dg-error "too many arguments to function" } */
  f1v(); /* { dg-error "too few arguments to function" } */
  x.f1v(); /* { dg-error "too few arguments to function" } */
  f1v(a);
  x.f1v(a);
  f1v(a, a);
  x.f1v(a, a);
  f2(a); /* { dg-error "too few arguments to function" } */
  x.f2(a); /* { dg-error "too few arguments to function" } */
  f2(a, a);
  x.f2(a, a);
  f2(a, a, a); /* { dg-error "too many arguments to function" } */
  x.f2(a, a, a); /* { dg-error "too many arguments to function" } */
  f2v(a); /* { dg-error "too few arguments to function" } */
  x.f2v(a); /* { dg-error "too few arguments to function" } */
  f2v(a, a);
  x.f2v(a, a);
  f2v(a, a, a);
  x.f2v(a, a, a);
}
