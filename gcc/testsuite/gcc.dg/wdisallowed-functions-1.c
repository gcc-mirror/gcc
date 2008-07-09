/* { dg-do compile } */
/* { dg-options "-Wdisallowed-function-list=foobar" } */

int foobar (int i)
{
  return (i * 5);
}
