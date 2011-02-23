// { dg-do compile }
// { dg-options "-std=gnu++0x" }

template<typename _Tp, _Tp v>
  struct A3
  {
    typedef _Tp value_type;
    typedef A3<value_type,v> type;

    static constexpr value_type value = v;

    constexpr operator value_type() { return value; }
  };

// Partial specialization.
template<typename _Tp, _Tp v>
  struct A3<_Tp*, v>
  {
    typedef _Tp* value_type;
    typedef A3<value_type,v> type;

    static constexpr value_type value = v;

    constexpr operator value_type() { return value; }
  };

// Explicit specialization.
template<>
  struct A3<unsigned short, 0>
  {
    typedef unsigned short value_type;
    typedef A3<value_type, 0> type;

    static constexpr value_type value = 0;

    constexpr operator value_type() { return value; }
  };

// Explicitly instantiate.
template struct A3<int, 415>;

// Extern explicitly instantiate.
extern template struct A3<int, 510>;

// Use.
A3<int, 1111> a31;
// FIXME should this be an error?
A3<char, 9999> a32;		// { dg-warning "overflow" }
