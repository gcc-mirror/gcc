// { dg-do run  }
template <class T, template <class T> class C>
struct X
{};

template <class T>
struct Y
{};

template <class T>
struct Z
{};

template <class T>
struct X<T,Y>
{};

int main()
{
  X<int,Y> a;
  X<int,Z> b;
}
