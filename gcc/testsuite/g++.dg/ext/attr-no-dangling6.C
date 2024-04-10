// PR c++/110358
// { dg-do compile { target c++20 } }
// { dg-options "-Wdangling-reference" }

class X { };
const X x1;
const X x2;

constexpr bool val () { return true; }
struct ST { static constexpr bool value = true; };
struct SF { static constexpr bool value = false; };

template<typename T>
[[gnu::no_dangling(T::value)]]
const X& get (const int& i)
{
   return i == 0 ? x1 : x2;
}

template<bool B = true>
[[gnu::no_dangling(B)]]
const X& foo (const int& i)
{
   return i == 0 ? x1 : x2;
}

[[gnu::no_dangling(val ())]]
const X& bar (const int& i)
{
   return i == 0 ? x1 : x2;
}

[[gnu::no_dangling(!val ())]]
const X& baz (const int& i)
{
   return i == 0 ? x1 : x2;
}

template <typename T>
struct [[gnu::no_dangling(T::value)]]
Span {
    T* data_;
    int len_;
    ~Span();

    [[nodiscard]] constexpr auto operator[](int n) const noexcept -> T& { return data_[n]; }
};

auto geti() -> Span<ST>;
auto gety() -> Span<SF>;

void
test ()
{
  [[maybe_unused]] const X& x1 = get<ST> (10);	  // { dg-bogus "dangling" }
  [[maybe_unused]] const X& x2 = get<SF> (10);	  // { dg-warning "dangling" }
  [[maybe_unused]] const X& x3 = foo<true> (10);  // { dg-bogus "dangling" }
  [[maybe_unused]] const X& x4 = foo<false> (10); // { dg-warning "dangling" }
  [[maybe_unused]] const X& x7 = foo<> (10);	  // { dg-bogus "dangling" }
  [[maybe_unused]] const X& x5 = bar (10);	  // { dg-bogus "dangling" }
  [[maybe_unused]] const X& x6 = baz (10);	  // { dg-warning "dangling" }

  [[maybe_unused]] const auto &b1 = geti()[0];	  // { dg-bogus "dangling" }
  [[maybe_unused]] const auto &b2 = gety()[0];	  // { dg-warning "dangling" }
}
