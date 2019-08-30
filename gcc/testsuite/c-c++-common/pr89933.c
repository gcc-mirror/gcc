/* PR c/89933 */
/* { dg-do compile } */

typedef unsigned int a __attribute__ ((__aligned__(8), __may_alias__));
typedef unsigned int a __attribute__ ((__aligned__(8), __may_alias__));
