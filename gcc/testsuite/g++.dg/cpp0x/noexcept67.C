// PR c++/100101
// { dg-do compile { target c++11 } }

template <typename T> struct A
{
    template <typename U> static char foo(U*, int* = 0);
    static const bool value = sizeof(foo(static_cast<T*>(nullptr))) > 0;
};

template <bool b> struct B
{
    static const bool value = b;
};

template <typename T> struct C
{
    typedef B<A<T>::value> type;
};

template <typename T>
void bar() noexcept(A<T>::value && C<T>::type::value) {}

void baz()
{
  bar<void>();
}
