// { dg-do compile { target c++17 } }

struct A { bool a, b; };
struct B { int a, b; };

void
foo ()
{
  auto [ a, b ] = A ();
  for (auto [ a, b ] = A (); a; )
    ;
  if (auto [ a, b ] = A (); a)
    ;
  switch (auto [ a, b ] = B (); b)
    {
    case 2:
      break;
    }
  auto && [ c, d ] = A ();
  [[maybe_unused]] auto [ e, f ] = A ();
  alignas (A) auto [ g, h ] = A ();
  __attribute__((unused)) auto [ i, j ] = A ();
}
