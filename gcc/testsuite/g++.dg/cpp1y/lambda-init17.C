// PR c++/71546
// { dg-do compile { target c++14 } }

namespace n { struct make_shared { }; }

int main()
{
  int x1;
  [e = n::make_shared (), x1]() {};
}
