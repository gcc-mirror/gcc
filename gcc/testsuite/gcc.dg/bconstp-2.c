/* As bconstp-1.c, but with the __builtin_constant_p calls
   parenthesized.  */
/* { dg-do compile } */

/* This test checks that builtin_constant_p can be used safely in
   initializers for static data.  The macro X() defined below should
   be an acceptable initializer expression no matter how complex its
   argument is.  */

extern int a;
extern int b;

extern int foo(void);
extern int bar(void);

#define X(exp) ((__builtin_constant_p(exp)) ? (exp) : -1)

const short tests[] = {
  X(0),
  X(a),
  X(0 && a),
  X(a && b),
  X(foo()),
  X(0 && foo()),
  X(a && foo()),
  X(foo() && bar())
};
