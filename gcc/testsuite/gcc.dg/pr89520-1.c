/* PR c/89520 */
/* { dg-do compile } */
/* { dg-options "-Ofast -w" } */

#define A(name) __typeof (__builtin_##name (0)) name (); long name##1 () { return name (); }
#define B(name) A(name) A(name##f) A(name##l)
B (ceil)
B (floor)
B (round)
B (trunc)
B (nearbyint)
B (rint)
B (logb)
