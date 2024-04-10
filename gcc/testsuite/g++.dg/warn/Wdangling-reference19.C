// PR c++/110358
// { dg-do compile { target c++11 } }
// { dg-options "-Wdangling-reference" }
// Like Wdangling-reference18.C but not actually a span-like class.

template <typename T>
struct Span {
    T* data_;
    int len_;
    ~Span ();

    [[nodiscard]] constexpr auto operator[](int n) const noexcept -> T& { return data_[n]; }
    [[nodiscard]] constexpr auto front() const noexcept -> T& { return data_[0]; }
    [[nodiscard]] constexpr auto back() const noexcept -> T& { return data_[len_ - 1]; }
};

auto get() -> Span<int>;

auto f() -> int {
    int const& a = get().front(); // { dg-warning "dangling reference" }
    int const& b = get().back();  // { dg-warning "dangling reference" }
    int const& c = get()[0];      // { dg-warning "dangling reference" }

    return a + b + c;
}
