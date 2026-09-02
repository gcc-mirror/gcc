// P3450R1 - Extend std::is_within_lifetime
// { dg-do compile { target c++23 } }
// { dg-skip-if "sizeof (bool) > 1" { powerpc-*-darwin* } }

namespace std {
  template <class T>
  consteval bool
  is_within_lifetime (const T *p) noexcept
  {
    return __builtin_is_within_lifetime (p);
  }
}

struct OptBool {
  union { bool b; char c; };

  // note: this assumes common implementation properties for bool and char:
  // * sizeof (bool) == sizeof (char), and
  // * the value representations for true and false are distinct
  //   from the value representation for 2
  constexpr OptBool () : c (2) { }
  constexpr OptBool (bool b) : b (b) { }

  constexpr bool has_value () const
  {
    if consteval
      {
	return std::is_within_lifetime (&b);	// during constant evaluation, cannot read from c
      }
    else
      {
	return c != 2;				// during runtime, must read from c
      }
  }

  constexpr const bool &operator * () const { return b; }
};

constexpr OptBool disengaged;
constexpr OptBool engaged (true);
static_assert (!disengaged.has_value ());
static_assert (engaged.has_value ());
static_assert (*engaged);
constexpr OptBool engaged2 (false);
static_assert (engaged2.has_value ());
static_assert (!*engaged2);
