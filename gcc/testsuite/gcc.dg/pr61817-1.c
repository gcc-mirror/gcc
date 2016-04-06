/* { dg-do compile } */
/* { dg-options "-std=c11 -ftrack-macro-expansion=0" } */

#define A(x) _Static_assert(x, #x)
#define F(x, y, z) a = __LINE__, b = x ## y, c = z

enum {
#line 10
    F
     (
      __LI,
      NE__,
      __LINE__
      )
};

A(a == 15);
A(b == 15);
A(c == 15);
