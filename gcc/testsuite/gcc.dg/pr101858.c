/* { dg-do compile } */
/* { dg-options "-w" } */

int foo(int a)
{
  if (a < (int*)((__INTPTR_TYPE__)1 << a))
    a = 0;
  return a;
}
