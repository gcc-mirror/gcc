// PR c++/116756
// { dg-do compile { target c++17 } }

template<int N> struct cst { static constexpr int value = N; };

struct Store {
  void openDF() {
    auto lambda = [this](auto& self, auto I) {
      if constexpr (I.value == 0) {
        auto next = [&self] { self(self, cst<1>{}); };
        openSeries(next);
      } else {
        openSeries(0);
      }
    };
    lambda(lambda, cst<0>{});
  }
  template<class T> void openSeries(T) { }
};

int main() {
  Store store;
  store.openDF();
}
