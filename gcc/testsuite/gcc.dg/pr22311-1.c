/* Bug 22311: ICE with -fshort-enums on shortened operations.  */
/* { dg-do compile } */
/* { dg-options "-fshort-enums" } */

typedef enum { A = 1 } E;
void f(E e, unsigned char c) { c |= e; }
