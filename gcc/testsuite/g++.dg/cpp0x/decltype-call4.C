// PR c++/81188
// { dg-do compile { target c++11 } }

template <class F>
struct C {
  F fast(long i) const;
  auto operator[](long i) const -> decltype(this->fast(i));
};

template <class F>
auto C<F>::operator[](long i) const -> decltype(this->fast(i)) {
  return fast(i);
}
