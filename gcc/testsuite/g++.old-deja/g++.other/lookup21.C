// Check that we don't complain about ambiguity between the same static
// member function in different subobjects.

struct A {
  static void f() {}
};

struct B: public A { };
struct C: public A { };
struct D: public B, public C { };

int main()
{
  D d;
  d.f();
}
