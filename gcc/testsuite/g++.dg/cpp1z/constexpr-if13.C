// PR c++/83273
// { dg-do compile { target c++17 } }

int main()
{
  auto d = 42;
  if constexpr (d > 0) {	// { dg-error "constant expression" }
      return d;
  }
}
