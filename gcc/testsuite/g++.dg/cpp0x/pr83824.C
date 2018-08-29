// PR c++/83824
// { dg-do compile { target c++11 } }

void
foo ()
{
  if (alignas(1 alignas(1)))	// { dg-error "expected" }
    ;
}
