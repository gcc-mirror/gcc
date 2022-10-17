// PR c++/102617
// P2360R0: Extend init-statement to allow alias-declaration
// { dg-do compile { target c++20 } }
// Test valid use.

int v[10];

void
g ()
{
  for (using T = int; (T) false;) // { dg-error "only available with" "" { target c++20_only } }
    ;
  for (using T = int; T e : v) // { dg-error "only available with" "" { target c++20_only } }
    (void) e;
  if (using T = int; true) // { dg-error "only available with" "" { target c++20_only } }
    {
      T x = 0;
      (void) x;
    }
  if constexpr (using T = int; true) // { dg-error "only available with" "" { target c++20_only } }
    {
      T x = 0;
      (void) x;
    }
  switch (using T = int; 42) // { dg-error "only available with" "" { target c++20_only } }
    case 42:
      {
	T x = 0;
	(void) x;
      }
}
