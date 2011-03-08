// { dg-do assemble  }

template<int M, int N>
class GCD {
public:
  enum { val = (N == 0) ? M : GCD<N, M % N>::val }; // { dg-warning "division" "division" }
// { dg-error "constant expression" "valid" { target *-*-* } 6 }
// { dg-message "template argument" "valid" { target *-*-* } 6 }
};

int main() {
  GCD< 1, 0 >::val; // { dg-message "instantiated" }
}
