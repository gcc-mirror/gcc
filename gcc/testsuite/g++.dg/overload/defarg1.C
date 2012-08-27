// PR c++/5247

template<typename T>
int foo (T t, int = foo(T()));	// { dg-error "recursive" }

struct A { };

int main()
{
  foo(A());			// { dg-message "default argument" }
}
