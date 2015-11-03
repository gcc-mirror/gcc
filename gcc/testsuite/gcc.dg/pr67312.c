/* { dg-do compile } */
/* { dg-options "-O0 -ftree-coalesce-vars" } */

void foo (int x, int y)
{
    y = x;
}
