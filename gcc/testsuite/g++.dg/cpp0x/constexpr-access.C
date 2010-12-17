// { dg-options -std=c++0x }

class base
{
protected:
  constexpr base() { }
};

struct A : base { };

int main()
{
  A a;
}
