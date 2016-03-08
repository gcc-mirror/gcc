// { dg-options "-std=c++1z -fconcepts" }

template <class T> concept bool Copyable = requires (T t) { T(t); };
template <class T> concept bool Constructable = requires { T(); };
template <class T> concept bool Both = Copyable<T> && Constructable<T>;

template <Copyable... Ts> void f(Ts...) { }
template <Both... Ts> void f(Ts...) { }

int main()
{
  f(42);
}
