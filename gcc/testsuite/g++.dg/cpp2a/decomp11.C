// PR c++/126057
// { dg-do compile { target c++20 } }

template <typename T>
int
foo ()
{
  static auto [a] = 0;	// { dg-error "cannot decompose non-array non-class type 'int'" }
  return 0;
}

int a = foo <int> ();
