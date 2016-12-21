/* PR driver/78863.  */

/* { dg-do compile } */
/* { dg-options "-fsanitize" } */
/* { dg-error "unrecognized command line option .-fsanitize..$" "" { target *-*-* } 0 } */
