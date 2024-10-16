/* PR c/71266 */
/* { dg-do compile } */
/* { dg-additional-options "-Wno-old-style-definition" } */

int fn1 (a)
  enum b {  /* { dg-warning "empty declaration" } */
    a /* { dg-error ".a. declared as a non-parameter" } */
  };
{
}
