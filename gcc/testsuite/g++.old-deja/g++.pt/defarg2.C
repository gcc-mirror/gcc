// { dg-do run  }
template <int S=0, class T=int>
struct X
{};

template <>
struct X<0,int>
{};

template <int S>
struct X<S,int>
: X<>
{};

int main()
{
  X<1,int> x;
}
