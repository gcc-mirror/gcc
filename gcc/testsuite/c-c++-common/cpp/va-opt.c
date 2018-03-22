/* { dg-do compile } */
/* { dg-options "-std=gnu99" { target c } } */
/* { dg-options "-std=c++2a" { target c++ } } */

extern void f0 (void);
extern void f1 (int);
extern void f2 (int, int);
extern void f3 (int, int, int);
extern void f4 (int, int, int, int);
extern int s (const char *);

#define CALL(F, ...) F (7 __VA_OPT__(,) __VA_ARGS__)
#define CP(F, X, Y, ...) F (__VA_OPT__(X ## Y,) __VA_ARGS__)
#define CS(F, ...) F(__VA_OPT__(s(# __VA_ARGS__)))
#define D(F, ...) F(__VA_OPT__(__VA_ARGS__) __VA_OPT__(,) __VA_ARGS__)
#define CALL0(...) __VA_OPT__(f2)(0 __VA_OPT__(,)__VA_ARGS__)

void t (void)
{
  CALL (f1);
  CALL (f1, );
  CALL (f2, 1);
  CALL (f3, 1, 2);

  int one = 1;
  int two = 2;
  int onetwo = 23;

  CP (f0, one, two);
  CP (f0, one, two, );
  CP (f2, one, two, 3);

  CS (f0);
  CS (f1, 1, 2, 3, 4);

  D (f0);
  D (f2, 1);
  D (f4, 1, 2);

  CALL0 ();
  CALL0 (23);
}
