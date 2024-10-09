// P2662R3 - Pack Indexing
// PR c++/113798
// { dg-do compile { target c++26 } }
// From LLVM's cxx2c-pack-indexing.cpp.

template<typename...>
struct X { };

template<typename... T>
requires requires(T...[0]) { {T...[0](0)}; }
struct S : T...[1] {
  [[maybe_unused]] T...[1] base = {};
  using foo = T...[1];
  S() : T...[1]() { }
  X<T...[0]> x;
  const T...[0] f(T...[0]&& parm) noexcept((T...[0])0) {
    T...[0] (*test)(const volatile T...[0]**);
    thread_local T...[0] d;
    [[maybe_unused]] T...[0] a = parm;
    auto ptr = new T...[0](0);
    (*ptr).~T...[0]();
    return T...[0](0);
    typename T...[1]::foo b = 0;
    T...[1]::i = 0;
    return (T...[0])(a);
    new T...[0];
    [[maybe_unused]] auto l = []<T...[0]>(T...[0][1]) -> T...[0]{ return {}; };
    [[maybe_unused]] auto _ = l.template operator()<T...[0]{}>({0});
  }
  operator T...[0]() const { }
};

struct base {
    using foo = int;
    static inline int i = 42;
};

int main()
{
  S<int, base>().f(0);
}
