// { dg-do run }

// PR tree-optimization/105769

// The partitioning code would incorrectly have bias
// and a temporary in the same partitioning because
// it was thought bias was not alive when those were alive
// do to vectorization of a store of pointers (that included bias).

#include <functional>

template<size_t n, class T>
struct vec {
  T dat[n];
  vec() {}
  explicit vec(const T& x) { for(size_t i = 0; i < n; i++) dat[i] = x; }
  T& operator [](size_t i) { return dat[i]; }
  const T& operator [](size_t i) const { return dat[i]; }
};

template<size_t m, size_t n, class T>
using mat = vec<m, vec<n, T>>;
template<size_t n, class T>
using sq_mat = mat<n, n, T>;
using map_t = std::function<size_t(size_t)>;
template<class T_v>
using est_t = std::function<T_v(map_t map)>;
template<class T_v> using est2_t = std::function<T_v(map_t map)>;
map_t id_map() { return [](size_t j) -> size_t { return j; }; }

template<size_t n, class T>
est2_t<void> jacknife(const est_t<vec<n, T>> est, sq_mat<n, T>& cov, vec<n, T>& bias) {
  return [est, &cov, &bias](map_t map) -> void
  {
        bias = est(map);
        for(size_t i = 0; i < n; i++)
        {
          bias[i].print();
        }
  };
}

template<class T>
void print_cov_ratio() {
  sq_mat<2, T> cov_jn;
  vec<2, T> bias;
  jacknife<2, T>([](map_t map) -> vec<2, T> { vec<2, T> retv; retv[0] = 1; retv[1] = 1; return retv; }, cov_jn, bias)(id_map());
}
struct ab {
  long long unsigned a;
  short unsigned b;
  double operator()() { return a; }
  ab& operator=(double rhs) { a = rhs; return *this; }
 void print();
};

void
ab::print()
{

}

int main() {
  print_cov_ratio<ab>();
  return 0;
}

