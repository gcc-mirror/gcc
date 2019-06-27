/* PR c++/90108 */
/* { dg-do compile } */
/* { dg-options "--param ggc-min-heapsize=0" } */

typedef unsigned int a __attribute__ ((__aligned__(8), __may_alias__));
typedef unsigned int a __attribute__ ((__aligned__(8), __may_alias__));
