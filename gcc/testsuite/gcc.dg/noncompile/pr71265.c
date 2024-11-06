/* PR c/71265 */
/* { dg-do compile } */
/* { dg-additional-options "-Wno-old-style-definition" } */

void ID (ID)
  int ID [__func__]; /* { dg-error "size of array .ID. has non-integer type" } */
{
}
