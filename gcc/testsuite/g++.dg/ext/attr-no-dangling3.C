// { dg-do compile { target c++11 } }
// { dg-options "-Wdangling-reference" }

template <typename T>
struct [[gnu::no_dangling]] Span {
    T* data_;
    int len_;
    // So that our heuristic doesn't suppress the warning anyway.
    ~Span();

    [[nodiscard]] constexpr auto operator[](int n) const noexcept -> T& { return data_[n]; }
    [[nodiscard]] constexpr auto front() const noexcept -> T& { return data_[0]; }
    [[nodiscard]] constexpr auto back() const noexcept -> T& { return data_[len_ - 1]; }
};

auto get() -> Span<int>;

auto f() -> int {
    int const& a = get().front(); // { dg-bogus "dangling" }
    int const& b = get().back();  // { dg-bogus "dangling" }
    int const& c = get()[0];      // { dg-bogus "dangling" }

    return a + b + c;
}
