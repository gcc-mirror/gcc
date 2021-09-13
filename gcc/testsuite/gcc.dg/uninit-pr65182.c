/* PR middle-end/65182 - -Wuninitialized fails when pointer to variable
   later passed to function
   { dg-do compile }
   { dg-options "-O0 -Wall" } */

void bar (int *a);

int baz (void);

__attribute__ ((noipa)) void foo_O0 (int *b)
{
  int a;

  if (a)            // { dg-warning "\\\[-Wuninitialized" }
    {
      *b = 0;
      return;
    }

  bar (&a);

  a = baz ();

  *b = a + 2;
}

#pragma GCC optimize ("2")

__attribute__ ((noipa)) void foo_O2 (int *b)
{
  int a;

  if (a)            // { dg-warning "\\\[-Wuninitialized" }
    {
      *b = 0;
      return;
    }

  bar (&a);

  a = baz ();

  *b = a + 3;
}
