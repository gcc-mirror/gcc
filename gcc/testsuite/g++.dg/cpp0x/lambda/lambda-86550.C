// PR c++/86550
// { dg-do compile { target c++11 } }

void
foo ()
{
  auto a = []() bool {};			// { dg-error "type-specifier invalid in lambda" }
  auto b = []() bool bool bool bool int {};	// { dg-error "type-specifier invalid in lambda" }
}
