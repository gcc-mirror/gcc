/* PR middle-end/90258.  */

/* { dg-do compile } */
/* { dg-options "-mandroidX" } */
/* { dg-error "unrecognized command line option '-mandroidX'; did you mean '-mandroid'"  "" { target *-*-* } 0 } */
