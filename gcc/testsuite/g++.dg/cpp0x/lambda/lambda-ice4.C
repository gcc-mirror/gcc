// PR c++/47242
// { dg-options "-std=c++0x" }

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
