// PR c++/5247

template<typename T>
int foo (T t, int = foo(T()));

struct A { };

int main()
{
  foo(A());			// { dg-error "default argument" }
}
