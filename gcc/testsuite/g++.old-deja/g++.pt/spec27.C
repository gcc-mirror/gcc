// Build don't link:

template <class T, class U>
struct Y {};

template <class T>
struct X {};

template <class T, class U>
void f()
{
  typename X<Y<T, U> >::A a;
}

template <class T, class U>
struct X<Y<T, U> >
{
};
