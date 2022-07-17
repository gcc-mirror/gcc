// { dg-do compile }
// { dg-options "-O3" }

enum E { E0, E1 };

void bar ();
void baz ();

int c;

void
foo (int i)
{
  E e = (E) i;
  while (c)
    switch (e)
      {
      case E0:
        bar ();
      case E1:
        baz ();
      }
}
