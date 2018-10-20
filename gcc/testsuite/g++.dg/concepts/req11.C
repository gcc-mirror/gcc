// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

// Check that we can evaluate constant requires-expressions
// as constant expressions, for the curious case when they
// appear within predicate constraints.

template<typename... Ts> struct variant { };

template<typename T>
concept bool Streamable()
{
  return requires (T t) { t; };
}

template<typename T>
concept bool Range()
{
  return requires (T t) { t; };
}

template<class T>
  requires Streamable<T>() and not Range<T>()
void print(const T& x) { }

int main()
{
  print("hello"); // { dg-error "cannot call" }
}
