// PR c++/51161
// { dg-options "-std=c++0x" }

struct A{};
struct B : A{};
struct C : A{};
struct D : B, C{};

int main()
{
  D d;
  static_cast<A &&>(d);		// { dg-error "ambiguous" }
}
