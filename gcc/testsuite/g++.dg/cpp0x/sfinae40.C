// PR c++/54541
// { dg-do compile { target c++11 } }

template <typename T> T&& declval();

struct X;

X f(int);

template <class T>
void g(decltype((void)f(declval<T>())) *)
{}

template <class T>
void g(...)
{}

int main()
{
  g<int>(0);
}
