// PR c++/89391
// { dg-do compile { target c++11 } }

struct S { };

void
foo ()
{
  auto a = reinterpret_cast<S&&>(foo ());	// { dg-error "invalid cast of an rvalue expression of type 'void' to type" }
}
