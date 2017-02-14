// Testcase from P0292R2
// { dg-do compile { target c++11 } }
// { dg-options "" }

template<typename T, typename ... Rest> void g(T&& p, Rest&& ...rs) {
  // ... handle p
  if constexpr (sizeof...(rs) > 0) // { dg-warning "constexpr" "" { target c++14_down } }
    g(rs...);  // never instantiated with an empty argument list.
}

int main()
{
  g(1,2,3);
}
