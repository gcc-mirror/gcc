// PR c++/89391
// { dg-do compile { target c++11 } }

struct S { };

void
foo ()
{
  auto a = reinterpret_cast<S&&>(foo ()); // { dg-error "12:invalid cast of a prvalue expression of type 'void' to type" }
}
