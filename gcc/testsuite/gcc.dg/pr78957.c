/* PR driver/78957 */
/* { dg-do compile } */
/* { dg-options "-fno-sso-struct=none" } */
/* { dg-error "unrecognized command-line option" "" { target *-*-* } 0 } */

int i;
