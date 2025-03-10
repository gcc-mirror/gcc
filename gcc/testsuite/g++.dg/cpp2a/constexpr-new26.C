// PR c++/119162
// { dg-do compile { target c++20 } }

constexpr int *
f7 ()
{
  int *p = new int (2);	// { dg-error "is not a constant expression because it refers to a result of" }
  delete p;
  return p;
}

void
g ()
{
  constexpr auto v7 = f7 ();
}

