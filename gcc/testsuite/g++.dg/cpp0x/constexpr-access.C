// { dg-options -std=c++11 }

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
