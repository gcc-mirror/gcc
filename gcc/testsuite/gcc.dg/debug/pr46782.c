/* PR debug/46782 */
/* { dg-do compile } */
/* { dg-options "-w -O0 -fvar-tracking -fcompare-debug" } */

void foo (int i)
{
  if (i)
    i++;
  while (i)
    ;
}
