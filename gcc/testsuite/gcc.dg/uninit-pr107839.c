/* { dg-do compile } */
/* { dg-options "-O2 -Wuninitialized" } */

int f (int);
void g (int c)
{
  int v;
  if (c)
    v = f(0);
  while (1)
    if (c)
      f(v + v); /* { dg-bogus "uninitialized" } */ 
}
