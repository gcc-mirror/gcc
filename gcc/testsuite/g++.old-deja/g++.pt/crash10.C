// Build don't link:

template<int M, int N>
class GCD {
public:
  enum { val = (N == 0) ? M : GCD<N, M % N>::val }; // ERROR - division
};

int main() {
  GCD< 1, 0 >::val; // ERROR - instantiated
}
