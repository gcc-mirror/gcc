/* Test messages for wrong number of arguments to function.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

void f0(void);
void f1(int);
void f1v(int, ...);
void f2(int, int);
void f2v(int, int, ...);

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
  f0(a); /* { dg-error "error: too many arguments to function 'f0'" } */
  x.f0(a); /* { dg-error "error: too many arguments to function 'x.f0'" } */
  f0(a, a); /* { dg-error "error: too many arguments to function 'f0'" } */
  x.f0(a, a); /* { dg-error "error: too many arguments to function 'x.f0'" } */
  f1(); /* { dg-error "error: too few arguments to function 'f1'" } */
  x.f1(); /* { dg-error "error: too few arguments to function 'x.f1'" } */
  f1(a);
  x.f1(a);
  f1(a, a); /* { dg-error "error: too many arguments to function 'f1'" } */
  x.f1(a, a); /* { dg-error "error: too many arguments to function 'x.f1'" } */
  f1v(); /* { dg-error "error: too few arguments to function 'f1v'" } */
  x.f1v(); /* { dg-error "error: too few arguments to function 'x.f1v'" } */
  f1v(a);
  x.f1v(a);
  f1v(a, a);
  x.f1v(a, a);
  f2(a); /* { dg-error "error: too few arguments to function 'f2'" } */
  x.f2(a); /* { dg-error "error: too few arguments to function 'x.f2'" } */
  f2(a, a);
  x.f2(a, a);
  f2(a, a, a); /* { dg-error "error: too many arguments to function 'f2'" } */
  x.f2(a, a, a); /* { dg-error "error: too many arguments to function 'x.f2'" } */
  f2v(a); /* { dg-error "error: too few arguments to function 'f2v'" } */
  x.f2v(a); /* { dg-error "error: too few arguments to function 'x.f2v'" } */
  f2v(a, a);
  x.f2v(a, a);
  f2v(a, a, a);
  x.f2v(a, a, a);
}
