// { dg-options "-std=c++11 -pedantic-errors" }

namespace A::B::C // { dg-error "nested namespace definitions only available with" }
{
}
