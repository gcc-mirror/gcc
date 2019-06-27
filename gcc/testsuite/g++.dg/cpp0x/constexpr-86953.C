// PR c++/86953
// { dg-do run { target c++11 } }
// { dg-options "-O2" }

struct B {
  double x;
  bool y, z;
  constexpr bool operator== (const B& o) const noexcept
  {
    return x == o.x && y == o.y && z == o.z;
  }
  constexpr bool operator!= (const B& o) const noexcept { return !(*this == o); }
};

int
main ()
{
  bool b = B{} == B{};
}
