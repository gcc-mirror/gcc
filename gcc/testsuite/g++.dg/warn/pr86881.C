// PR c++/86881 ICE with shadow warning
// { dg-do compile { target c++11 } }
// { dg-additional-options { -Wshadow-compatible-local } }}

void a() {
  auto b([] {});
  {
    auto b = 0;
  }
}

struct Proxy { };

void Two ()
{
  auto my = Proxy ();
  {
    auto my = Proxy (); // { dg-warning "shadows" "" { xfail *-*-* } }
  };
}
