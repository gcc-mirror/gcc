/* { dg-do compile } */
/* { dg-options "-Os -fno-asynchronous-unwind-tables -g" } */

void bar (long double n);

void foo (int c)
{
  if (c)
    bar (0);
}
