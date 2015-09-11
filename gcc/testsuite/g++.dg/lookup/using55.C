// PR c++/65721

template<typename T>
struct A {
  typedef T D;
};

template<typename X>
class B : public A<X> {
  using typename B::D;		// { dg-error "not a base" }
public:
  D echo(D x) {			// { dg-error "D" }
    return x;
  }
};

int main() {
  B<int> b;
}
