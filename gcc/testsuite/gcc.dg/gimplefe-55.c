/* { dg-do compile } */
/* { dg-options "-fgimple" } */

/* PR c/117749 */
/* Make sure we don't ICE after not have a full local
   declaration in gimple fe. */

__GIMPLE
void foo ( ) {
  int ;  /* { dg-error "" } */
}
