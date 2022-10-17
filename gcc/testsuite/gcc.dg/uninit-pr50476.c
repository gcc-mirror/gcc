/* PR middle-end/50476 - Warn of pointer set to object whose lifetime is limited
   { dg-do compile }
   { dg-options "-O1 -Wall" } */

int *x = 0;

void f (void)
{
  int y = 1;
  x = &y;       // { dg-warning "\\\[-Wdangling-pointer" }
}

int g (void)
{
  f ();

  return *x;    // { dg-warning "\\\[-Wuninitialized" }
}
