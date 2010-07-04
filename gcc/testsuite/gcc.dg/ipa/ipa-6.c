/* PR middle-end/29122 */
/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp -fno-early-inlining" } */

int
dont_inline (int);

int
bar (int b, int c)
{
   return dont_inline (c);
}

int
foo (int a)
{
  if (a++ > 0)
    bar (a, 3);

  foo (7);
}

int
main ()
{
  int i;
  for (i = 0; i < 100; i++)
    foo (7);
  return 0;
}



