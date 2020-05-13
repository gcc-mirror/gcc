// { dg-do compile { target c++20 } }
// { dg-final { scan-assembler "_Z1gI1XEvT_" } }
// { dg-final { scan-assembler "_Z1gI1YEvT_" } }
// { dg-final { scan-assembler "_Z1gIiEvT_" } }

template<typename T>
  concept C = __is_class(T);

template<typename T>
  concept D = C<T> && __is_empty(T);

struct X { };
struct Y { int n; };

template<typename T> void g(T) { } // #1
template<C T> void g(T) { } // #2
template<D T> void g(T) { } // #3

template void g(int); // Instantiate #1
template void g(X); // Instantitae #3
template void g(Y); // Instantiate #2

int main() { }
