// { dg-do compile { target concepts } }

template <class T> void f(T t)
  requires requires { static_cast<T&&>(t); }
{}

int main()
{
  f(42);
}
