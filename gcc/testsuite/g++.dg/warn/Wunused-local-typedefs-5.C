// PR c++/123277
// { dg-do compile { target c++11 } }
// { dg-options "-Wunused-local-typedefs" }

template <typename T>
void
foo (T B)
{
  typedef T C [[maybe_unused]];
  typedef T D [[gnu::unused]];
  typedef T E [[gnu::used]];
  typedef T F;
  typedef T G;			// { dg-warning "typedef 'G' locally defined but not used" }
  F f;
}
