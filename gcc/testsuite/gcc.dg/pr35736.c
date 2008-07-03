/* { dg-do compile } */
/* { dg-options "-O -Wall" } */

void foo()
{
  while (1)
    for (;;({ continue; }))
      ;
}
