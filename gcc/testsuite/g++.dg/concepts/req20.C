// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template <class T> concept bool C = true;

template <class T>
requires C<typename T::foo>
void f(T t) { }

void f(...);

template <class T>
requires C<T>
void g(T t) { }

int main()
{
  f(42);
  g(42);
}

