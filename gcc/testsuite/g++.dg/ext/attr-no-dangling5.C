// PR c++/110358
// { dg-do compile { target c++20 } }
// { dg-options "-Wdangling-reference" }

template <typename T>
struct Span {
    T* data_;
    int len_;
    ~Span();

    [[nodiscard]] constexpr auto operator[](int n) const noexcept -> T& { return data_[n]; }
};

template <>
struct [[gnu::no_dangling]] Span<int> {
    int* data_;
    int len_;
    ~Span();

    [[nodiscard]] constexpr auto operator[](int n) const noexcept -> int& { return data_[n]; }
};

auto getch() -> Span<char>;
auto geti() -> Span<int>;

void
f ()
{
  [[maybe_unused]] const auto &a = getch()[0];	// { dg-warning "dangling reference" }
  [[maybe_unused]] const auto &b = geti()[0];   // { dg-bogus "dangling reference" }
}
