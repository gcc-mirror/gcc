/* PR 17635 */
/* Contributed by Devang Patel  <dpatel@apple.com>  */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

void foo(int i)
{
  while (1)
    if (i) ++i;
}

