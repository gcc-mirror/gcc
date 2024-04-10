// tests for ops that must be member functions are seperate

// the name of the class refers to the type of it's member functions xobj parameter

#define MAKE_STRUCT_OPS(TYPE)					\
  TYPE operator+=(this TYPE self, int) { return self; }		\
  TYPE operator-=(this TYPE self, int) { return self; }		\
  TYPE operator*=(this TYPE self, int) { return self; }		\
  TYPE operator/=(this TYPE self, int) { return self; }		\
  TYPE operator%=(this TYPE self, int) { return self; }		\
  TYPE operator&=(this TYPE self, int) { return self; }		\
  TYPE operator|=(this TYPE self, int) { return self; }		\
  TYPE operator^=(this TYPE self, int) { return self; }		\
  TYPE operator<<=(this TYPE self, int) { return self; }	\
  TYPE operator>>=(this TYPE self, int) { return self; }	\
  TYPE operator++(this TYPE self) { return self; }		\
  TYPE operator--(this TYPE self) { return self; }		\
  TYPE operator++(this TYPE self, int) { return self; }		\
  TYPE operator--(this TYPE self, int) { return self; }		\
  TYPE operator+(this TYPE self) { return self; }		\
  TYPE operator-(this TYPE self) { return self; }		\
  TYPE operator+(this TYPE self, int) { return self; }		\
  TYPE operator-(this TYPE self, int) { return self; }		\
  TYPE operator*(this TYPE self, int) { return self; }		\
  TYPE operator/(this TYPE self, int) { return self; }		\
  TYPE operator%(this TYPE self, int) { return self; }		\
  TYPE operator&(this TYPE self, int) { return self; }		\
  TYPE operator|(this TYPE self, int) { return self; }		\
  TYPE operator^(this TYPE self, int) { return self; }		\
  TYPE operator<<(this TYPE self, int) { return self; }		\
  TYPE operator>>(this TYPE self, int) { return self; }		\
  TYPE operator!(this TYPE self) { return self; }		\
  TYPE operator&&(this TYPE self, int const&) { return self; }	\
  TYPE operator||(this TYPE self, int const&) { return self; }	\
  TYPE operator==(this TYPE self, int) { return self; }		\
  TYPE operator!=(this TYPE self, int) { return self; }		\
  TYPE operator<(this TYPE self, int) { return self; }		\
  TYPE operator>(this TYPE self, int) { return self; }		\
  TYPE operator<=(this TYPE self, int) { return self; }		\
  TYPE operator>=(this TYPE self, int) { return self; }		\
  TYPE operator<=>(this TYPE self, int) { return self; }	\
  TYPE operator*(this TYPE self) { return self; }		\
  TYPE operator->*(this TYPE self, int) { return self; }	\
  TYPE operator&(this TYPE self) { return self; }		\
  TYPE operator,(this TYPE self, int) { return self; }

struct Value {
  MAKE_STRUCT_OPS (Value)
};

struct LRef {
  MAKE_STRUCT_OPS (LRef&)
};

struct RRef {
  MAKE_STRUCT_OPS (RRef&&)
};

struct ConstLRef {
  MAKE_STRUCT_OPS (ConstLRef const&)
};

struct ConstRRef {
  MAKE_STRUCT_OPS (ConstRRef const&&)
};

#undef MAKE_STRUCT_OPS

struct Deduced {
  template<typename Self> Self&& operator+=(this Self&& self, int) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator-=(this Self&& self, int) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator*=(this Self&& self, int) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator/=(this Self&& self, int) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator%=(this Self&& self, int) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator&=(this Self&& self, int) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator|=(this Self&& self, int) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator^=(this Self&& self, int) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator<<=(this Self&& self, int) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator>>=(this Self&& self, int) { return static_cast<Self&&>(self); }

  template<typename Self> Self&& operator++(this Self&& self) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator--(this Self&& self) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator++(this Self&& self, int) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator--(this Self&& self, int) { return static_cast<Self&&>(self); }

  template<typename Self> Self&& operator+(this Self&& self) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator-(this Self&& self) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator+(this Self&& self, int) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator-(this Self&& self, int) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator*(this Self&& self, int) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator/(this Self&& self, int) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator%(this Self&& self, int) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator&(this Self&& self, int) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator|(this Self&& self, int) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator^(this Self&& self, int) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator<<(this Self&& self, int) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator>>(this Self&& self, int) { return static_cast<Self&&>(self); }

  template<typename Self> Self&& operator!(this Self&& self) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator&&(this Self&& self, int const&) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator||(this Self&& self, int const&) { return static_cast<Self&&>(self); }

  template<typename Self> Self&& operator==(this Self&& self, int) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator!=(this Self&& self, int) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator<(this Self&& self, int) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator>(this Self&& self, int) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator<=(this Self&& self, int) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator>=(this Self&& self, int) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator<=>(this Self&& self, int) { return static_cast<Self&&>(self); }

  template<typename Self> Self&& operator*(this Self&& self) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator->*(this Self&& self, int) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator&(this Self&& self) { return static_cast<Self&&>(self); }
  template<typename Self> Self&& operator,(this Self&& self, int) { return static_cast<Self&&>(self); }
};

#define TEST_OPS(OPERAND) \
  (OPERAND) += 0;	\
  (OPERAND) -= 0;	\
  (OPERAND) *= 0;	\
  (OPERAND) /= 0;	\
  (OPERAND) %= 0;	\
  (OPERAND) &= 0;	\
  (OPERAND) |= 0;	\
  (OPERAND) ^= 0;	\
  (OPERAND) <<= 0;	\
  (OPERAND) >>= 0;	\
			\
  ++(OPERAND);		\
  --(OPERAND);		\
  (OPERAND)++;		\
  (OPERAND)--;		\
			\
  +(OPERAND);		\
  -(OPERAND);		\
  (OPERAND) + 0;	\
  (OPERAND) - 0;	\
  (OPERAND) * 0;	\
  (OPERAND) / 0;	\
  (OPERAND) % 0;	\
  (OPERAND) & 0;	\
  (OPERAND) | 0;	\
  (OPERAND) ^ 0;	\
  (OPERAND) << 0;	\
  (OPERAND) >> 0;	\
			\
  !(OPERAND);		\
  (OPERAND) && 0;	\
  (OPERAND) || 0;	\
			\
  (OPERAND) == 0;	\
  (OPERAND) != 0;	\
  (OPERAND) < 0;	\
  (OPERAND) > 0;	\
  (OPERAND) <= 0;	\
  (OPERAND) >= 0;	\
  (OPERAND) <=> 0;	\
			\
  *(OPERAND);		\
  (OPERAND) ->* 0;	\
  &(OPERAND);		\
  (OPERAND), 0;

#define VALIDATE_RETURN_TYPES(OPERAND, CORRECT_TYPE) \
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) += 0)));		\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) -= 0)));		\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) *= 0)));		\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) /= 0)));		\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) %= 0)));		\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) &= 0)));		\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) |= 0)));		\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) ^= 0)));		\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) <<= 0)));		\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) >>= 0)));		\
										\
  static_assert(__is_same(CORRECT_TYPE, decltype(++(OPERAND))));		\
  static_assert(__is_same(CORRECT_TYPE, decltype(--(OPERAND))));		\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND)++)));		\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND)--)));		\
										\
  static_assert(__is_same(CORRECT_TYPE, decltype(+(OPERAND))));			\
  static_assert(__is_same(CORRECT_TYPE, decltype(-(OPERAND))));			\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) + 0)));		\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) - 0)));		\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) * 0)));		\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) / 0)));		\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) % 0)));		\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) & 0)));		\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) | 0)));		\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) ^ 0)));		\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) << 0)));		\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) >> 0)));		\
										\
  static_assert(__is_same(CORRECT_TYPE, decltype(!(OPERAND))));			\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) && 0)));		\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) || 0)));		\
										\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) == 0)));		\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) != 0)));		\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) < 0)));		\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) > 0)));		\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) <= 0)));		\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) >= 0)));		\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) <=> 0)));		\
										\
  static_assert(__is_same(CORRECT_TYPE, decltype(*(OPERAND))));			\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND) ->* 0)));		\
  static_assert(__is_same(CORRECT_TYPE, decltype(&(OPERAND))));			\
  static_assert(__is_same(CORRECT_TYPE, decltype((OPERAND), 0)));

