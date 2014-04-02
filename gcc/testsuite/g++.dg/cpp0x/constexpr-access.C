// { dg-do compile { target c++11 } }

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
