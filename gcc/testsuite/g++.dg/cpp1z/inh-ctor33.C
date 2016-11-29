// { dg-do link { target c++11 } }
// { dg-options -fnew-inheriting-ctors }

struct A
{
  A() { }
  A(const A&);			// should never be called
};

struct B
{
  B(A) { }
};

struct C: B
{
  using B::B;
};

int main()
{
  C c{A()};
}
