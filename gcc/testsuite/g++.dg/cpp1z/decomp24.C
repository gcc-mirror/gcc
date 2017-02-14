// PR c++/78896
// { dg-do compile { target c++11 } }
// { dg-options "" }

int
foo ()
{
  int a {10};
  auto [b] { [&a](){} };	// { dg-error "cannot decompose lambda closure type" }
  return b - a;			// { dg-warning "decomposition declaration only available with" "" { target c++14_down } .-1 }
}
