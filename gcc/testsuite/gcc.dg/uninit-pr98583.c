/* PR middle-end/98583 - missing -Wuninitialized reading from a second VLA
   in its own block
   { dg-do compile }
   { dg-options "-O2 -Wall" }
   { dg-require-effective-target alloca } */

void f (int*);
void g (int);

void h1 (int n)
{
  int a[n];
  f (a);

  int b[n];
  g (b[1]);         // { dg-warning "\\\[-Wuninitialized" }
}

void h2 (int n, int i, int j)
{
  if (i)
    {
      int a[n];
      f (a);
    }

  if (j)
    {
      int b[n];
      g (b[1]);     // { dg-warning "\\\[-Wmaybe-uninitialized" }
    }
}
