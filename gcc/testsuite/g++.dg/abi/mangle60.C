// PR c++/51854
// { dg-options "-fabi-compat-version=0" }

template <unsigned N> struct A;

template <typename U, typename V>
char foo(U, V);

// { dg-final { scan-assembler "_Z3barIiEvP1AIXszcl3foocvT__ELCi0_42EEEE" } }
template <typename U>
void bar(A<sizeof(foo(U(), 42i))> *);

// { dg-final { scan-assembler "_Z3bazIiEvP1AIXszcl3foocvT__ELCf00000000_00000000EEEE" } }
template <typename U>
void baz(A<sizeof(foo(U(), 0.0fj))> *);

int main() {
   bar<int>(0);
   baz<int>(0);
}
