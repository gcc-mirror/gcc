// PR c++/47242
// { dg-do compile { target c++11 } }

template < typename > void
bar ()
{
  [i]{}; // { dg-error "declared|invalid" }
}

void
foo ()
{
  bar<int>();
}
