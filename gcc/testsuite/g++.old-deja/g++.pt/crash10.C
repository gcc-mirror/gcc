// { dg-do assemble  }

template<int M, int N>
class GCD {
public:
  enum { val = (N == 0) ? M : GCD<N, M % N>::val }; // { dg-error "" } division
};

int main() {
  GCD< 1, 0 >::val; // { dg-error "" } instantiated
}
