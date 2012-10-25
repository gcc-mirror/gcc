// DR 1402
// { dg-do compile { target c++11 } }

struct A
{
  A& operator=(A&&);
};

struct B: virtual A { B& operator=(B&&) = default; }; // { dg-warning "virtual base" }
struct C: virtual A { };			      // { dg-warning "virtual base" }

int main()
{
  B b1, b2;
  b2 = static_cast<B&&>(b1);

  C c1, c2;
  c2 = static_cast<C&&>(c1);
}
