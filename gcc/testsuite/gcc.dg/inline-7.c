/* Test duplicate inline, gnu99 mode.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -fgnu89-inline" } */

inline inline void f (void) {}
