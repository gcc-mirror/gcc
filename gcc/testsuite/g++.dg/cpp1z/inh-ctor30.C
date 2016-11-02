// { dg-do compile { target c++11 } }

struct A
{
  A(double);
};

struct B: A
{
  B(short);
  using A::A;
};

int main()
{
  B b(1);			// { dg-error "ambiguous" }
}
