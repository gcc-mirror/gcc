// PR c++/115361
// { dg-additional-options -Wdangling-reference }

struct B { int i; };

struct A {
  const int & operator()(const B& b) { return b.i; }
};

int main()
{
  B b = {};
  const int &r = A()(b);
}
