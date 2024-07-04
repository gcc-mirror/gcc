// PR c++/109640
// { dg-do compile { target c++20 } }
// { dg-options "-Wdangling-reference" }
// Don't warn for std::span-like classes.

#include <iterator>
#include <span>

template <typename T>
struct MySpan
{
 MySpan(T* data, std::size_t size) :
    data_(data),
    size_(size)
 {}

 T& operator[](std::size_t idx) { return data_[idx]; }

private:
    T* data_;
    std::size_t size_;
};

template <typename T, std::size_t n>
MySpan<T const> make_my_span(T const(&x)[n])
{
    return MySpan(std::begin(x), n);
}

template <typename T, std::size_t n>
std::span<T const> make_span(T const(&x)[n])
{
    return std::span(std::begin(x), n);
}

int main()
{
  int x[10]{};
  [[maybe_unused]] int const& y1{make_my_span(x)[0]};
  [[maybe_unused]] int const& y2{make_span(x)[0]};
  using T = int[10];
  [[maybe_unused]] int const& y3{make_my_span(T{})[0]}; // { dg-warning "dangling reference" }
  [[maybe_unused]] int const& y4{make_span(T{})[0]};	// { dg-warning "dangling reference" }
}
