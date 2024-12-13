// { dg-do compile { target c++11 } }

struct R{};
bool operator! (R); // { dg-message "candidate .:" }

template <typename T> bool Foo (T x)
{
  return [](T x) 
	 { return !x; }(x); // { dg-error "no match for 'operator!'" }
}

namespace X 
{
struct S {};
}

// not found by adl :)
bool operator! (X::S);

int main ()
{
  Foo (X::S{});
}
