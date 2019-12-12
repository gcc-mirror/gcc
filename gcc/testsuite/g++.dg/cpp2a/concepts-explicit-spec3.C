// { dg-do compile { target c++2a } }

template<typename T>
  concept C = __is_class(T);

template<C T> struct S;

struct X { };

// Not a valid explicit specialization, int does not satisfy C.
template<> struct S<int> { }; // { dg-error "constraint failure" }

int main() { }
