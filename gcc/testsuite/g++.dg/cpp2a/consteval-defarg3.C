// DR 2631: default args and DMI aren't immediately evaluated
// { dg-do compile { target c++20 } }
// { dg-final { scan-assembler-not "foober" } }

consteval int foober();

int g(int = foober());
struct A { int i = foober(); };
template <int i = foober()> struct B { };
struct C
{
  consteval C(int = foober()) { }
};
int h(C = C());

consteval int foober() { return 42; }

int main() {
  A a;
  B<> b;
  g();
  h();
}
