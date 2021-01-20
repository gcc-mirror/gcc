/* PR tree-optimization/98721 */
/* { dg-do compile } */

int
foo (void)
{
  return __builtin_strlen (__builtin_alloca_with_align (0, 16));	/* { dg-warning "'__builtin_strlen' reading 1 or more bytes from a region of size 0" } */
}	/* { dg-message "source object '<unknown>' of size 0" "" { target *-*-* } .-1 } */
