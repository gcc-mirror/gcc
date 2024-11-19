/* PR c/117641 */
/* { dg-do compile { target bitint575 } } */
/* { dg-options "-std=c23" } */

void
foo (_BitInt(128) *b)
{
  __sync_fetch_and_add (b, 1);	/* { dg-error "incompatible" "" { target { ! int128 } } } */
}
