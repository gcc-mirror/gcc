/* PR c/71265 */
/* { dg-do compile } */

void ID (ID)
  int ID [__func__]; /* { dg-error "size of array .ID. has non-integer type" } */
{
}
