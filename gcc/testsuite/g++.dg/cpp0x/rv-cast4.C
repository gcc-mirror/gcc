// PR c++/51161
// { dg-do compile { target c++11 } }

struct A{};
struct B : A{};
struct C : A{};
struct D : B, C{};

int main()
{
  D d;
  static_cast<A &&>(d);		// { dg-error "ambiguous" }
}
