/* PR c/77323 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "" } */

__int128 a; /* { dg-error "not supported" } */
_Float128x b; /* { dg-error "not supported" } */
