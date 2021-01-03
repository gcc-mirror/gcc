// { dg-do compile { target c++11 } }
// PR c++/81589

template <typename k>
struct z {
  z() noexcept {
    k::error;
  }
};

int x = __is_nothrow_constructible(z<int>);
