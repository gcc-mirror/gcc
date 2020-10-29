// { dg-do assemble  }

template<int M, int N>
class GCD {
public:
  enum { val = (N == 0) ? M : GCD<N, M % N>::val };
// { dg-error "constant expression" "valid" { target *-*-* } .-1 }
// { dg-message "template argument" "valid" { target *-*-* } .-2 }
};

int main() {
  GCD< 1, 0 >::val; // { dg-message "required" }
}
