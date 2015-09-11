// PR c++/50800

template <typename T> struct B;
template <typename T> struct B<T &> {
  typedef T type;
};
struct A {
  typedef int TA __attribute__((__may_alias__));
};
void d() { B<int &> b; }
int main() { B<A::TA &> b; }	// { dg-warning "attributes" }
