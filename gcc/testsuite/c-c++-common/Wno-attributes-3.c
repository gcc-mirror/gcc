/* PR c++/101940 */
/* { dg-do compile } */
/* { dg-additional-options "-Wno-attributes=___::" } */
/* { dg-additional-options "-Wno-attributes=c::____" } */
/* { dg-additional-options "-Wno-attributes=____::____" } */
/* { dg-additional-options "-Wno-attributes=c@::attr" } */
/* { dg-additional-options "-Wno-attributes=c2::@tr" } */

/* { dg-error "wrong argument to ignored attributes" "" { target *-*-* } 0 } */
