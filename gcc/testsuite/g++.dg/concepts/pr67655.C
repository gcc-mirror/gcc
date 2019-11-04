// { dg-do compile { target c++11 } }
// { dg-additional-options "-fconcepts" }

template<class... Xs>
void consume(Xs&&...) {}

template<class... Xs>
  struct A {
    template<class... Ys>
    requires requires(Ys... ys) {
      consume(Xs{ys}...);
    }
  A(Ys&&... ys) {
  }
  A(int) {}
};

int main() {
  A<int, double> a(55);
}
