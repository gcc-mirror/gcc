// { dg-do compile { target c++11 } }

struct [[nodiscard]] A { };	// { dg-message "" }

A f();				// { dg-message "" }

int main()
{
  f();				// { dg-warning "Wunused-result" }
}
