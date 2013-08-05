// PR c++/57901
// { dg-require-effective-target c++11 }

struct Z {
    Z()         = default;
    Z(Z const&) = default;
    constexpr Z(Z&&) {} /* non-trivial (constexpr) move ctor */
};

template<typename T>
constexpr int fn0(T v) { return 0; }
template<typename T>
constexpr int fn (T v) { return fn0(v); }

constexpr auto t0 = fn0(Z()); // OK!
constexpr auto t  = fn (Z()); // error! (GCC 4.8.1)
