/* Test duplicate inline, gnu89 mode.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu89" } */

inline inline void f (void) {}
