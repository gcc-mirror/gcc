// PR c++/79304
// { dg-do compile }

struct C { };

template<class T>
struct X
{
  C* c;

  void f() {
    this->c.s();	// { dg-error "->c" }
  }
};

int main()
{
  X<int> x;
  x.f();
}
