// PR c++/81589

template <typename k>
struct z {
  z() {
    k::error;
  }
};

int x = __is_trivially_constructible(z<int>);
