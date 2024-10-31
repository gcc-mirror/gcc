/* N3356 - if declarations.  */
/* PR c/117019 */
/* { dg-do run } */
/* { dg-options "-std=c2y" } */

int g;
int get () { ++g; return 42; }

struct S { int i; };

int
main ()
{
  if (auto x = get (); get (), x);
  if (g != 2)
    __builtin_abort ();

  switch (auto x = get (); get (), x);
  if (g != 4)
    __builtin_abort ();

  if (struct S s = { 42 }; s.i != 42)
    __builtin_abort ();

  if (int i = 42)
    {
      i = 0;
      if (int j = 42)
	j = 0;
      else
	j = 42;
    }
  else
    i = 42;
}
