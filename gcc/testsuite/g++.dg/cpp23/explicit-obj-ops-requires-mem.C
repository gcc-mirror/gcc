// P0847R7
// { dg-do compile { target c++23 } }

// well-formed and ill-formed uses of member only operators in a requires expression

// suppress the warning for Value's arrow operator
// { dg-options "-Wno-return-local-addr" }

// It's very hard to test for incorrect successes without requires, and by extension a non dependent variable
// so for the time being, there are no non dependent tests invalid calls.

struct Value {
  int _v;
  Value operator=(this Value self, int) { return self; }
  Value operator()(this Value self) { return self; }
  Value operator[](this Value self) { return self; }
  Value* operator->(this Value self) { return &self; }
};

struct LRef {
  int _v;
  LRef& operator=(this LRef& self, int) { return self; }
  LRef& operator()(this LRef& self) { return self; }
  LRef& operator[](this LRef& self) { return self; }
  LRef* operator->(this LRef& self) { return &self; }
};

struct RRef {
  int _v;
  RRef&& operator=(this RRef&& self, int) { return static_cast<RRef&&>(self); }
  RRef&& operator()(this RRef&& self) { return static_cast<RRef&&>(self); }
  RRef&& operator[](this RRef&& self) { return static_cast<RRef&&>(self); }
  RRef* operator->(this RRef&& self) { return &self; }
};

struct ConstLRef {
  int _v;
  ConstLRef const& operator=(this ConstLRef const& self, int) { return self; }
  ConstLRef const& operator()(this ConstLRef const& self) { return self; }
  ConstLRef const& operator[](this ConstLRef const& self) { return self; }
  ConstLRef const* operator->(this ConstLRef const& self) { return &self; }
};

struct ConstRRef {
  int _v;
  ConstRRef const&& operator=(this ConstRRef const&& self, int) { return static_cast<ConstRRef const&&>(self); }
  ConstRRef const&& operator()(this ConstRRef const&& self) { return static_cast<ConstRRef const&&>(self); }
  ConstRRef const&& operator[](this ConstRRef const&& self) { return static_cast<ConstRRef const&&>(self); }
  ConstRRef const* operator->(this ConstRRef const&& self) { return &self; }
};

// needed to implement deduced operator->
template<typename T> struct remove_ref { using type = T; };
template<typename T> struct remove_ref<T&> { using type = T; };
template<typename T> struct remove_ref<T&&> { using type = T; };
template<typename T> using remove_ref_t = typename remove_ref<T>::type;

struct Deduced {
  int _v;
  template<typename Self>
  Self&& operator=(this Self&& self, int) { return static_cast<Self&&>(self); }
  template<typename Self>
  Self&& operator()(this Self&& self) { return static_cast<Self&&>(self); }
  template<typename Self>
  Self&& operator[](this Self&& self) { return static_cast<Self&&>(self); }
  template<typename Self>
  remove_ref_t<Self>* operator->(this Self&& self) { return &self; }
};

#define TEST_INVALID(OPERAND) \
  static_assert(!requires{ (OPERAND) = 0; }, "Unexpected success calling operator = with " #OPERAND);	\
  static_assert(!requires{ (OPERAND)(); }, "Unexpected success calling operator () with " #OPERAND);	\
  static_assert(!requires{ (OPERAND)[]; }, "Unexpected success calling operator [] with " #OPERAND);	\
  static_assert(!requires{ (OPERAND)->_v; }, "Unexpected success calling operator -> with " #OPERAND);

#define TEST_VALID(OPERAND) \
  static_assert(requires{ (OPERAND) = 0; }, "Unexpected failure calling operator = with " #OPERAND);	\
  static_assert(requires{ (OPERAND)(); }, "Unexpected failure calling operator () with " #OPERAND);	\
  static_assert(requires{ (OPERAND)[]; }, "Unexpected failure calling operator [] with " #OPERAND);	\
  static_assert(requires{ (OPERAND)->_v; }, "Unexpected failure calling operator -> with " #OPERAND);

template<typename T, typename U>
concept same_as = __is_same(T, U);

#define TEST_VALID_WITH_RETURN_TYPES(OPERAND, CORRECT_TYPE) \
  static_assert(requires{ {(OPERAND) = 0} -> same_as<CORRECT_TYPE>; },"Unexpected failure with return type check calling operator = with " #OPERAND " -> expected return type: " #CORRECT_TYPE);	\
  static_assert(requires{ {(OPERAND)()} -> same_as<CORRECT_TYPE>; },  "Unexpected failure with return type check calling operator () with " #OPERAND " -> expected return type: " #CORRECT_TYPE);	\
  static_assert(requires{ {(OPERAND)[]} -> same_as<CORRECT_TYPE>; },  "Unexpected failure with return type check calling operator [] with " #OPERAND " -> expected return type: " #CORRECT_TYPE);
  

template<typename DepValue = Value>
void test_value()
{
  DepValue value{};
  TEST_VALID(value)
  TEST_VALID(static_cast<DepValue&&>(value))
  TEST_VALID(static_cast<DepValue const&>(value))
  TEST_VALID(static_cast<DepValue const&&>(value))
}

template<typename DepLRef = LRef>
void test_l_ref()
{
  DepLRef l_ref{};
  TEST_VALID(l_ref)
  TEST_INVALID(static_cast<DepLRef&&>(l_ref))
  TEST_INVALID(static_cast<DepLRef const&>(l_ref))
  TEST_INVALID(static_cast<DepLRef const&&>(l_ref))
}

template<typename DepRRef = RRef>
void test_r_ref()
{
  DepRRef r_ref{};
  TEST_INVALID(r_ref)
  TEST_VALID(static_cast<DepRRef&&>(r_ref))
  TEST_INVALID(static_cast<DepRRef const&>(r_ref))
  TEST_INVALID(static_cast<DepRRef const&&>(r_ref))
}

template<typename DepConstLRef = ConstLRef>
void test_const_l_ref()
{
  DepConstLRef const_l_ref{};
  TEST_VALID(const_l_ref)
  TEST_VALID(static_cast<DepConstLRef&&>(const_l_ref))
  TEST_VALID(static_cast<DepConstLRef const&>(const_l_ref))
  TEST_VALID(static_cast<DepConstLRef const&&>(const_l_ref))
}

template<typename DepConstRRef = ConstRRef>
void test_const_r_ref()
{
  DepConstRRef const_r_ref{};
  TEST_INVALID(const_r_ref)
  TEST_VALID(static_cast<DepConstRRef&&>(const_r_ref))
  TEST_INVALID(static_cast<DepConstRRef const&>(const_r_ref))
  TEST_VALID(static_cast<DepConstRRef const&&>(const_r_ref))
}

template<typename DepDeduced = Deduced>
void test_deduced()
{
  DepDeduced deduced{};

  TEST_VALID(deduced)
  TEST_VALID(static_cast<DepDeduced&&>(deduced))
  TEST_VALID(static_cast<DepDeduced const&>(deduced))
  TEST_VALID(static_cast<DepDeduced const&&>(deduced))

  TEST_VALID_WITH_RETURN_TYPES(deduced, DepDeduced&)
  TEST_VALID_WITH_RETURN_TYPES(static_cast<DepDeduced&&>(deduced), DepDeduced&&)
  TEST_VALID_WITH_RETURN_TYPES(static_cast<DepDeduced const&>(deduced), DepDeduced const&)
  TEST_VALID_WITH_RETURN_TYPES(static_cast<DepDeduced const&&>(deduced), DepDeduced const&&)
  // arrow operator needs to be seperate to check the type of _v
  static_assert(requires{ {(deduced->_v)} -> same_as<int&>; }, "Unexpected failure with return type check calling operator -> with deduced->_v");
  static_assert(requires{ {(static_cast<DepDeduced&&>(deduced)->_v)} -> same_as<int&>; }, "Unexpected failure with return type check calling operator -> with static_cast<DepDeduced&&>(deduced)->_v");
  static_assert(requires{ {(static_cast<DepDeduced const&>(deduced)->_v)} -> same_as<int const&>; }, "Unexpected failure with return type check calling operator -> with static_cast<DepDeduced const&>(deduced)->_v");
  static_assert(requires{ {(static_cast<DepDeduced const&&>(deduced)->_v)} -> same_as<int const&>; }, "Unexpected failure with return type check calling operator -> with static_cast<DepDeduced const&&>(deduced)->_v");
}

void test()
{
  test_value();
  test_l_ref();
  test_r_ref();
  test_const_l_ref();
  test_const_r_ref();
  test_deduced();
}

