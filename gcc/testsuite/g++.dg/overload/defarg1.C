// PR c++/5247

template<typename T>
int foo (T t, int = foo(T()));

int main()
{
  foo(0);			// { dg-error "default argument" }
}
