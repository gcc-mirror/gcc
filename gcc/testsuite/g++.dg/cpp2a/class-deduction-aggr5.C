// PR c++/95568
// { dg-do compile { target c++20 } }

template<typename T> struct X { T x; };
template<typename T, typename U> struct X2 { T x; U y; };
template<typename T> concept Y = requires { X{0}; };

template<typename T>
void g()
{
  X{0};
  X2{1, 2.2};
  Y auto y = X{1};
}

void
fn ()
{
  g<int>();
}
