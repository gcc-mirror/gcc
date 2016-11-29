// { dg-options -std=c++1z }

template <template <class> class T>
void f()
{
  T t = 42;			// { dg-error "B" }
};

template <class T>
struct A
{
  A(T);
};

template <class T> using B = T;

int main()
{
  f<A>();
  f<B>();			// { dg-message "here" }
}
