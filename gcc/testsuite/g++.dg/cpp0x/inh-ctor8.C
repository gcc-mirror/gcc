// { dg-options "-std=c++11" }

struct A
{
  int i;
  explicit A(int i): i(i) {}
};

struct B: A
{
  using A::A;
};

void f(B);

int main()
{
  f(B(42));			// OK
  f(42);			// { dg-error "could not convert" }
}
