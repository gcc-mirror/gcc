/* { dg-do compile } */
/* { dg-options "--param ggc-min-expand=0 --param ggc-min-heapsize=0" } */
static __inline__ int f () { return g (); }
int g () { return f (); }
