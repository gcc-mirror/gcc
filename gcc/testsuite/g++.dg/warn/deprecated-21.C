// { dg-do compile { target c++11 } }

[[deprecated]] void depr_fn() {}

template <class T> void f(T) {
  depr_fn();			// { dg-warning "deprecated" }
}

int main()
{
  f(42);
}
