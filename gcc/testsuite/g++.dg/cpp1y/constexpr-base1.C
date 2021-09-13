// PR c++/100209
// { dg-do compile { target c++14 } }

template<typename Derived>
struct __a_t
{
  unsigned char A = 0;
  constexpr Derived & SetA(const unsigned char & value) {
    A = value;
    return *static_cast<Derived *>(this);
  }
};

template<typename Derived>
struct __b_t
{
  unsigned char B = 0;
  constexpr Derived & SetB(const unsigned char & value) {
    B = value;
    return *static_cast<Derived *>(this);
  }
};

struct __ab_t : __a_t<__ab_t>, __b_t<__ab_t> { };

constexpr auto AB = __ab_t().SetA(100).SetB(10);
static_assert(AB.A == 100, "");
static_assert(AB.B == 10, "");
