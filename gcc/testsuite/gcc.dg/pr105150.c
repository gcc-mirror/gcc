/* PR tree-optimization/105150 */
/* { dg-options "-std=gnu17 -w -Ofast" } */

#define A(name) __typeof (__builtin_##name (0)) name (); \
  float name##1 () { return !name (1); } \
  double name##2 () { return name (1.0L); }
#define B(name) A(name) A(name##l)
B (sqrt)
