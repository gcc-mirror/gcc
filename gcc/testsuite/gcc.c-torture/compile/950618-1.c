/* { dg-additional-options "-std=gnu89" } */

static __inline__ int f () { return g (); }
int g () { return f (); }
