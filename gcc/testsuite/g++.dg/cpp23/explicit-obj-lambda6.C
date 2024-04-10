// P0847R7
// { dg-do compile { target c++23 } }

// decltype((x)) and decltype(x) in explicit object lambda

template<typename T> inline constexpr bool is_const_v = false;
template<typename T> inline constexpr bool is_const_v<T const> = true;

template<typename T> inline constexpr bool is_lvalue_ref = false;
template<typename T> inline constexpr bool is_lvalue_ref<T&> = true;

void non_dep()
{
  int n = 0;
  int const c = 0;
  // value
  auto f0_value = [=]<typename Self>(this Self){
    static_assert(is_lvalue_ref<decltype((n))>,
		  "decltype((n)) is not an lvalue ref");
    static_assert(is_const_v<Self> == is_const_v<__remove_reference (decltype((n)))>, // { dg-bogus {static assertion failed: qualification of decltype\(\(n\)\) does not match qualification of Self} }
		  "qualification of decltype((n)) does not match qualification of Self");
    static_assert(__is_same (__remove_cvref (decltype((n))), int),
		  "decltype((n)) is not an int");
    static_assert(__is_same (decltype(n), int));

    static_assert(is_lvalue_ref<decltype((c))>,
		  "decltype((c)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((c)))>,
		  "qualification of decltype((c)) is not const");
    static_assert(__is_same (__remove_cvref (decltype((c))), int),
		  "decltype((c)) is not an int");
    static_assert(__is_same (decltype(c), int const));
  };
  f0_value();
  f0_value.operator()<decltype(f0_value) const>();

  auto f1_value = [&]<typename Self>(this Self){
    static_assert(__is_same (decltype((n)), int&));
    static_assert(__is_same (decltype(n), int));

    static_assert(__is_same (decltype((c)), int const&));
    static_assert(__is_same (decltype(c), int const));
  };
  f1_value();
  f1_value.operator()<decltype(f1_value) const>();

  auto f2_value = [n, c]<typename Self>(this Self){
    static_assert(is_lvalue_ref<decltype((n))>,
		  "decltype((n)) is not an lvalue ref");
    static_assert(is_const_v<Self> == is_const_v<__remove_reference (decltype((n)))>, // { dg-bogus {static assertion failed: qualification of decltype\(\(n\)\) does not match qualification of Self} }
		  "qualification of decltype((n)) does not match qualification of Self");
    static_assert(__is_same (__remove_cvref (decltype((n))), int),
		  "decltype((n)) is not an int");
    static_assert(__is_same (decltype(n), int));

    static_assert(is_lvalue_ref<decltype((c))>,
		  "decltype((c)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((c)))>,
		  "qualification of decltype((c)) is not const");
    static_assert(__is_same (__remove_cvref (decltype((c))), int),
		  "decltype((c)) is not an int");
    static_assert(__is_same (decltype(c), int const));
  };
  f2_value();
  f2_value.operator()<decltype(f2_value) const>();

  auto f3_value = [&n, &c]<typename Self>(this Self){
    static_assert(__is_same (decltype((n)), int&));
    static_assert(__is_same (decltype(n), int));

    static_assert(__is_same (decltype((c)), int const&));
    static_assert(__is_same (decltype(c), int const));
  };
  f3_value();
  f3_value.operator()<decltype(f3_value) const>();

  auto f4_value = []<typename Self>(this Self){
    static_assert(__is_same (decltype(n), int));
    static_assert(__is_same (decltype((n)), int&));

    static_assert(__is_same (decltype(c), int const));
    static_assert(__is_same (decltype((c)), int const&));
  };
  f4_value();
  f4_value.operator()<decltype(f4_value) const>();

  // ref
  auto f0_ref = [=]<typename Self>(this Self&){
    static_assert(is_lvalue_ref<decltype((n))>,
		  "decltype((n)) is not an lvalue ref");
    static_assert(is_const_v<Self> == is_const_v<__remove_reference (decltype((n)))>, // { dg-bogus {static assertion failed: qualification of decltype\(\(n\)\) does not match qualification of Self} }
		  "qualification of decltype((n)) does not match qualification of Self");
    static_assert(__is_same (__remove_cvref (decltype((n))), int),
		  "decltype((n)) is not an int");
    static_assert(__is_same (decltype(n), int));

    static_assert(is_lvalue_ref<decltype((c))>,
		  "decltype((c)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((c)))>,
		  "qualification of decltype((c)) is not const");
    static_assert(__is_same (__remove_cvref (decltype((c))), int),
		  "decltype((c)) is not an int");
    static_assert(__is_same (decltype(c), int const));
  };
  f0_ref();
  f0_ref.operator()<decltype(f0_ref) const>();
  static_cast<decltype(f0_ref) const&>(f0_ref)();

  auto f1_ref = [&]<typename Self>(this Self&){
    static_assert(__is_same (decltype((n)), int&));
    static_assert(__is_same (decltype(n), int));

    static_assert(__is_same (decltype((c)), int const&));
    static_assert(__is_same (decltype(c), int const));
  };
  f1_ref();
  f1_ref.operator()<decltype(f1_ref) const>();
  static_cast<decltype(f1_ref) const&>(f1_ref)();

  auto f2_ref = [n, c]<typename Self>(this Self&){
    static_assert(is_lvalue_ref<decltype((n))>,
		  "decltype((n)) is not an lvalue ref");
    static_assert(is_const_v<Self> == is_const_v<__remove_reference (decltype((n)))>, // { dg-bogus {static assertion failed: qualification of decltype\(\(n\)\) does not match qualification of Self} }
		  "qualification of decltype((n)) does not match qualification of Self");
    static_assert(__is_same (__remove_cvref (decltype((n))), int),
		  "decltype((n)) is not an int");
    static_assert(__is_same (decltype(n), int));

    static_assert(is_lvalue_ref<decltype((c))>,
		  "decltype((c)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((c)))>,
		  "qualification of decltype((c)) is not const");
    static_assert(__is_same (__remove_cvref (decltype((c))), int),
		  "decltype((c)) is not an int");
    static_assert(__is_same (decltype(c), int const));
  };
  f2_ref();
  f2_ref.operator()<decltype(f2_ref) const>();
  static_cast<decltype(f2_ref) const&>(f2_ref)();

  auto f3_ref = [&n, &c]<typename Self>(this Self&){
    static_assert(__is_same(decltype((n)), int&));
    static_assert(__is_same (decltype(n), int));

    static_assert(__is_same (decltype((c)), int const&));
    static_assert(__is_same (decltype(c), int const));
  };
  f3_ref();
  f3_ref.operator()<decltype(f3_ref) const>();
  static_cast<decltype(f3_ref) const&>(f3_ref)();

  auto f4_ref = []<typename Self>(this Self&){
    static_assert(__is_same (decltype(n), int));
    static_assert(__is_same (decltype((n)), int&));

    static_assert(__is_same (decltype(c), int const));
    static_assert(__is_same (decltype((c)), int const&));
  };
  f4_ref();
  f4_ref.operator()<decltype(f4_ref) const>();
  static_cast<decltype(f4_ref) const&>(f4_ref)();

  // const value
  auto f0_const_value = [=]<typename Self>(this Self const){
    static_assert(is_lvalue_ref<decltype((n))>,
		  "decltype((n)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((n)))>, // { dg-bogus {static assertion failed: qualification of decltype\(\(n\)\) does not match qualification of Self} }
		  "qualification of decltype((n)) does not match qualification of Self");
    static_assert(__is_same (__remove_cvref (decltype((n))), int),
		  "decltype((n)) is not an int");
    static_assert(__is_same (decltype(n), int));

    static_assert(is_lvalue_ref<decltype((c))>,
		  "decltype((c)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((c)))>,
		  "qualification of decltype((c)) is not const");
    static_assert(__is_same (__remove_cvref (decltype((c))), int),
		  "decltype((c)) is not an int");
    static_assert(__is_same (decltype(c), int const));
  };
  f0_const_value();

  auto f1_const_value = [&]<typename Self>(this Self const){
    static_assert(__is_same (decltype((n)), int&));
    static_assert(__is_same (decltype(n), int));

    static_assert(__is_same (decltype((c)), int const&));
    static_assert(__is_same (decltype(c), int const));
  };
  f1_const_value();

  auto f2_const_value = [n, c]<typename Self>(this Self const){
    static_assert(is_lvalue_ref<decltype((n))>,
		  "decltype((n)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((n)))>, // { dg-bogus {static assertion failed: qualification of decltype\(\(n\)\) does not match qualification of Self} }
		  "qualification of decltype((n)) does not match qualification of Self");
    static_assert(__is_same (__remove_cvref (decltype((n))), int),
		  "decltype((n)) is not an int");
    static_assert(__is_same (decltype(n), int));

    static_assert(is_lvalue_ref<decltype((c))>,
		  "decltype((c)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((c)))>,
		  "qualification of decltype((c)) is not const");
    static_assert(__is_same (__remove_cvref (decltype((c))), int),
		  "decltype((c)) is not an int");
    static_assert(__is_same (decltype(c), int const));
  };
  f2_const_value();

  auto f3_const_value = [&n, &c]<typename Self>(this Self const){
    static_assert(__is_same (decltype((n)), int&));
    static_assert(__is_same (decltype(n), int));

    static_assert(__is_same (decltype((c)), int const&));
    static_assert(__is_same (decltype(c), int const));
  };
  f3_const_value();

  auto f4_const_value = []<typename Self>(this Self const){
    static_assert(__is_same (decltype(n), int));
    static_assert(__is_same (decltype((n)), int&));

    static_assert(__is_same (decltype(c), int const));
    static_assert(__is_same (decltype((c)), int const&));
  };
  f4_const_value();

  // const ref
  auto f0_const_ref = [=]<typename Self>(this Self const&){
    static_assert(is_lvalue_ref<decltype((n))>,
		  "decltype((n)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((n)))>, // { dg-bogus {static assertion failed: qualification of decltype\(\(n\)\) does not match qualification of Self} }
		  "qualification of decltype((n)) does not match qualification of Self");
    static_assert(__is_same (__remove_cvref (decltype((n))), int),
		  "decltype((n)) is not an int");
    static_assert(__is_same (decltype(n), int));

    static_assert(is_lvalue_ref<decltype((c))>,
		  "decltype((c)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((c)))>,
		  "qualification of decltype((c)) is not const");
    static_assert(__is_same (__remove_cvref (decltype((c))), int),
		  "decltype((c)) is not an int");
    static_assert(__is_same (decltype(c), int const));
  };
  f0_const_ref();

  auto f1_const_ref = [&]<typename Self>(this Self const&){
    static_assert(__is_same (decltype((n)), int&));
    static_assert(__is_same (decltype(n), int));

    static_assert(__is_same (decltype((c)), int const&));
    static_assert(__is_same (decltype(c), int const));
  };
  f1_const_ref();

  auto f2_const_ref = [n, c]<typename Self>(this Self const&){
    static_assert(is_lvalue_ref<decltype((n))>,
		  "decltype((n)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((n)))>, // { dg-bogus {static assertion failed: qualification of decltype\(\(n\)\) does not match qualification of Self} }
		  "qualification of decltype((n)) does not match qualification of Self");
    static_assert(__is_same (__remove_cvref (decltype((n))), int),
		  "decltype((n)) is not an int");
    static_assert(__is_same (decltype(n), int));

    static_assert(is_lvalue_ref<decltype((c))>,
		  "decltype((c)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((c)))>,
		  "qualification of decltype((c)) is not const");
    static_assert(__is_same (__remove_cvref (decltype((c))), int),
		  "decltype((c)) is not an int");
    static_assert(__is_same (decltype(c), int const));
  };
  f2_const_ref();

  auto f3_const_ref = [&n, &c]<typename Self>(this Self const&){
    static_assert(__is_same (decltype((n)), int&));
    static_assert(__is_same (decltype(n), int));

    static_assert(__is_same (decltype((c)), int const&));
    static_assert(__is_same (decltype(c), int const));
  };
  f3_const_ref();

  auto f4_const_ref = []<typename Self>(this Self const&){
    static_assert(__is_same (decltype(n), int));
    static_assert(__is_same (decltype((n)), int&));

    static_assert(__is_same (decltype(c), int const));
    static_assert(__is_same (decltype((c)), int const&));
  };
  f4_const_ref();
}

template<typename = void>
void dep0()
{
  int n = 0;
  int const c = 0;
  // value
  auto f0_value = [=]<typename Self>(this Self){
    static_assert(is_lvalue_ref<decltype((n))>,
		  "decltype((n)) is not an lvalue ref");
    static_assert(is_const_v<Self> == is_const_v<__remove_reference (decltype((n)))>, // { dg-bogus {static assertion failed: qualification of decltype\(\(n\)\) does not match qualification of Self} }
		  "qualification of decltype((n)) does not match qualification of Self");
    static_assert(__is_same (__remove_cvref (decltype((n))), int),
		  "decltype((n)) is not an int");
    static_assert(__is_same (decltype(n), int));

    static_assert(is_lvalue_ref<decltype((c))>,
		  "decltype((c)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((c)))>,
		  "qualification of decltype((c)) is not const");
    static_assert(__is_same (__remove_cvref (decltype((c))), int),
		  "decltype((c)) is not an int");
    static_assert(__is_same (decltype(c), int const));
  };
  f0_value();
  f0_value.template operator()<decltype(f0_value) const>();

  auto f1_value = [&]<typename Self>(this Self){
    static_assert(__is_same (decltype((n)), int&));
    static_assert(__is_same (decltype(n), int));

    static_assert(__is_same (decltype((c)), int const&));
    static_assert(__is_same (decltype(c), int const));
  };
  f1_value();
  f1_value.template operator()<decltype(f1_value) const>();

  auto f2_value = [n, c]<typename Self>(this Self){
    static_assert(is_lvalue_ref<decltype((n))>,
		  "decltype((n)) is not an lvalue ref");
    static_assert(is_const_v<Self> == is_const_v<__remove_reference (decltype((n)))>, // { dg-bogus {static assertion failed: qualification of decltype\(\(n\)\) does not match qualification of Self} }
		  "qualification of decltype((n)) does not match qualification of Self");
    static_assert(__is_same (__remove_cvref (decltype((n))), int),
		  "decltype((n)) is not an int");
    static_assert(__is_same (decltype(n), int));

    static_assert(is_lvalue_ref<decltype((c))>,
		  "decltype((c)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((c)))>,
		  "qualification of decltype((c)) is not const");
    static_assert(__is_same (__remove_cvref (decltype((c))), int),
		  "decltype((c)) is not an int");
    static_assert(__is_same (decltype(c), int const));
  };
  f2_value();
  f2_value.template operator()<decltype(f2_value) const>();

  auto f3_value = [&n, &c]<typename Self>(this Self){
    static_assert(__is_same (decltype((n)), int&));
    static_assert(__is_same (decltype(n), int));

    static_assert(__is_same (decltype((c)), int const&));
    static_assert(__is_same (decltype(c), int const));
  };
  f3_value();
  f3_value.template operator()<decltype(f3_value) const>();

  auto f4_value = []<typename Self>(this Self){
    static_assert(__is_same (decltype(n), int));
    static_assert(__is_same (decltype((n)), int&));

    static_assert(__is_same (decltype(c), int const));
    static_assert(__is_same (decltype((c)), int const&));
  };
  f4_value();
  f4_value.template operator()<decltype(f4_value) const>();

  // ref
  auto f0_ref = [=]<typename Self>(this Self&){
    static_assert(is_lvalue_ref<decltype((n))>,
		  "decltype((n)) is not an lvalue ref");
    static_assert(is_const_v<Self> == is_const_v<__remove_reference (decltype((n)))>, // { dg-bogus {static assertion failed: qualification of decltype\(\(n\)\) does not match qualification of Self} }
		  "qualification of decltype((n)) does not match qualification of Self");
    static_assert(__is_same (__remove_cvref (decltype((n))), int),
		  "decltype((n)) is not an int");
    static_assert(__is_same (decltype(n), int));

    static_assert(is_lvalue_ref<decltype((c))>,
		  "decltype((c)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((c)))>,
		  "qualification of decltype((c)) is not const");
    static_assert(__is_same (__remove_cvref (decltype((c))), int),
		  "decltype((c)) is not an int");
    static_assert(__is_same (decltype(c), int const));
  };
  f0_ref();
  f0_ref.template operator()<decltype(f0_ref) const>();
  static_cast<decltype(f0_ref) const&>(f0_ref)();

  auto f1_ref = [&]<typename Self>(this Self&){
    static_assert(__is_same (decltype((n)), int&));
    static_assert(__is_same (decltype(n), int));

    static_assert(__is_same (decltype((c)), int const&));
    static_assert(__is_same (decltype(c), int const));
  };
  f1_ref();
  f1_ref.template operator()<decltype(f1_ref) const>();
  static_cast<decltype(f1_ref) const&>(f1_ref)();

  auto f2_ref = [n, c]<typename Self>(this Self&){
    static_assert(is_lvalue_ref<decltype((n))>,
		  "decltype((n)) is not an lvalue ref");
    static_assert(is_const_v<Self> == is_const_v<__remove_reference (decltype((n)))>, // { dg-bogus {static assertion failed: qualification of decltype\(\(n\)\) does not match qualification of Self} }
		  "qualification of decltype((n)) does not match qualification of Self");
    static_assert(__is_same (__remove_cvref (decltype((n))), int),
		  "decltype((n)) is not an int");
    static_assert(__is_same (decltype(n), int));

    static_assert(is_lvalue_ref<decltype((c))>,
		  "decltype((c)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((c)))>,
		  "qualification of decltype((c)) is not const");
    static_assert(__is_same (__remove_cvref (decltype((c))), int),
		  "decltype((c)) is not an int");
    static_assert(__is_same (decltype(c), int const));
  };
  f2_ref();
  f2_ref.template operator()<decltype(f2_ref) const>();
  static_cast<decltype(f2_ref) const&>(f2_ref)();

  auto f3_ref = [&n, &c]<typename Self>(this Self&){
    static_assert(__is_same(decltype((n)), int&));
    static_assert(__is_same (decltype(n), int));

    static_assert(__is_same (decltype((c)), int const&));
    static_assert(__is_same (decltype(c), int const));
  };
  f3_ref();
  f3_ref.template operator()<decltype(f3_ref) const>();
  static_cast<decltype(f3_ref) const&>(f3_ref)();

  auto f4_ref = []<typename Self>(this Self&){
    static_assert(__is_same (decltype(n), int));
    static_assert(__is_same (decltype((n)), int&));

    static_assert(__is_same (decltype(c), int const));
    static_assert(__is_same (decltype((c)), int const&));
  };
  f4_ref();
  f4_ref.template operator()<decltype(f4_ref) const>();
  static_cast<decltype(f4_ref) const&>(f4_ref)();

  // const value
  auto f0_const_value = [=]<typename Self>(this Self const){
    static_assert(is_lvalue_ref<decltype((n))>,
		  "decltype((n)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((n)))>, // { dg-bogus {static assertion failed: qualification of decltype\(\(n\)\) does not match qualification of Self} }
		  "qualification of decltype((n)) does not match qualification of Self");
    static_assert(__is_same (__remove_cvref (decltype((n))), int),
		  "decltype((n)) is not an int");
    static_assert(__is_same (decltype(n), int));

    static_assert(is_lvalue_ref<decltype((c))>,
		  "decltype((c)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((c)))>,
		  "qualification of decltype((c)) is not const");
    static_assert(__is_same (__remove_cvref (decltype((c))), int),
		  "decltype((c)) is not an int");
    static_assert(__is_same (decltype(c), int const));
  };
  f0_const_value();

  auto f1_const_value = [&]<typename Self>(this Self const){
    static_assert(__is_same (decltype((n)), int&));
    static_assert(__is_same (decltype(n), int));

    static_assert(__is_same (decltype((c)), int const&));
    static_assert(__is_same (decltype(c), int const));
  };
  f1_const_value();

  auto f2_const_value = [n, c]<typename Self>(this Self const){
    static_assert(is_lvalue_ref<decltype((n))>,
		  "decltype((n)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((n)))>, // { dg-bogus {static assertion failed: qualification of decltype\(\(n\)\) does not match qualification of Self} }
		  "qualification of decltype((n)) does not match qualification of Self");
    static_assert(__is_same (__remove_cvref (decltype((n))), int),
		  "decltype((n)) is not an int");
    static_assert(__is_same (decltype(n), int));

    static_assert(is_lvalue_ref<decltype((c))>,
		  "decltype((c)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((c)))>,
		  "qualification of decltype((c)) is not const");
    static_assert(__is_same (__remove_cvref (decltype((c))), int),
		  "decltype((c)) is not an int");
    static_assert(__is_same (decltype(c), int const));
  };
  f2_const_value();

  auto f3_const_value = [&n, &c]<typename Self>(this Self const){
    static_assert(__is_same (decltype((n)), int&));
    static_assert(__is_same (decltype(n), int));

    static_assert(__is_same (decltype((c)), int const&));
    static_assert(__is_same (decltype(c), int const));
  };
  f3_const_value();

  auto f4_const_value = []<typename Self>(this Self const){
    static_assert(__is_same (decltype(n), int));
    static_assert(__is_same (decltype((n)), int&));

    static_assert(__is_same (decltype(c), int const));
    static_assert(__is_same (decltype((c)), int const&));
  };
  f4_const_value();

  // const ref
  auto f0_const_ref = [=]<typename Self>(this Self const&){
    static_assert(is_lvalue_ref<decltype((n))>,
		  "decltype((n)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((n)))>, // { dg-bogus {static assertion failed: qualification of decltype\(\(n\)\) does not match qualification of Self} }
		  "qualification of decltype((n)) does not match qualification of Self");
    static_assert(__is_same (__remove_cvref (decltype((n))), int),
		  "decltype((n)) is not an int");
    static_assert(__is_same (decltype(n), int));

    static_assert(is_lvalue_ref<decltype((c))>,
		  "decltype((c)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((c)))>,
		  "qualification of decltype((c)) is not const");
    static_assert(__is_same (__remove_cvref (decltype((c))), int),
		  "decltype((c)) is not an int");
    static_assert(__is_same (decltype(c), int const));
  };
  f0_const_ref();

  auto f1_const_ref = [&]<typename Self>(this Self const&){
    static_assert(__is_same (decltype((n)), int&));
    static_assert(__is_same (decltype(n), int));

    static_assert(__is_same (decltype((c)), int const&));
    static_assert(__is_same (decltype(c), int const));
  };
  f1_const_ref();

  auto f2_const_ref = [n, c]<typename Self>(this Self const&){
    static_assert(is_lvalue_ref<decltype((n))>,
		  "decltype((n)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((n)))>, // { dg-bogus {static assertion failed: qualification of decltype\(\(n\)\) does not match qualification of Self} }
		  "qualification of decltype((n)) does not match qualification of Self");
    static_assert(__is_same (__remove_cvref (decltype((n))), int),
		  "decltype((n)) is not an int");
    static_assert(__is_same (decltype(n), int));

    static_assert(is_lvalue_ref<decltype((c))>,
		  "decltype((c)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((c)))>,
		  "qualification of decltype((c)) is not const");
    static_assert(__is_same (__remove_cvref (decltype((c))), int),
		  "decltype((c)) is not an int");
    static_assert(__is_same (decltype(c), int const));
  };
  f2_const_ref();

  auto f3_const_ref = [&n, &c]<typename Self>(this Self const&){
    static_assert(__is_same (decltype((n)), int&));
    static_assert(__is_same (decltype(n), int));

    static_assert(__is_same (decltype((c)), int const&));
    static_assert(__is_same (decltype(c), int const));
  };
  f3_const_ref();

  auto f4_const_ref = []<typename Self>(this Self const&){
    static_assert(__is_same (decltype(n), int));
    static_assert(__is_same (decltype((n)), int&));

    static_assert(__is_same (decltype(c), int const));
    static_assert(__is_same (decltype((c)), int const&));
  };
  f4_const_ref();
}

// dep1 uses the template parameter

template<typename T = int>
void dep1()
{
  T n = 0;
  T const c = 0;
  // value
  auto f0_value = [=]<typename Self>(this Self){
    static_assert(is_lvalue_ref<decltype((n))>,
		  "decltype((n)) is not an lvalue ref");
    static_assert(is_const_v<Self> == is_const_v<__remove_reference (decltype((n)))>, // { dg-bogus {static assertion failed: qualification of decltype\(\(n\)\) does not match qualification of Self} }
		  "qualification of decltype((n)) does not match qualification of Self");
    static_assert(__is_same (__remove_cvref (decltype((n))), int),
		  "decltype((n)) is not an int");
    static_assert(__is_same (decltype(n), int));

    static_assert(is_lvalue_ref<decltype((c))>,
		  "decltype((c)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((c)))>,
		  "qualification of decltype((c)) is not const");
    static_assert(__is_same (__remove_cvref (decltype((c))), int),
		  "decltype((c)) is not an int");
    static_assert(__is_same (decltype(c), int const));
  };
  f0_value();
  f0_value.template operator()<decltype(f0_value) const>();

  auto f1_value = [&]<typename Self>(this Self){
    static_assert(__is_same (decltype((n)), int&));
    static_assert(__is_same (decltype(n), int));

    static_assert(__is_same (decltype((c)), int const&));
    static_assert(__is_same (decltype(c), int const));
  };
  f1_value();
  f1_value.template operator()<decltype(f1_value) const>();

  auto f2_value = [n, c]<typename Self>(this Self){
    static_assert(is_lvalue_ref<decltype((n))>,
		  "decltype((n)) is not an lvalue ref");
    static_assert(is_const_v<Self> == is_const_v<__remove_reference (decltype((n)))>, // { dg-bogus {static assertion failed: qualification of decltype\(\(n\)\) does not match qualification of Self} }
		  "qualification of decltype((n)) does not match qualification of Self");
    static_assert(__is_same (__remove_cvref (decltype((n))), int),
		  "decltype((n)) is not an int");
    static_assert(__is_same (decltype(n), int));

    static_assert(is_lvalue_ref<decltype((c))>,
		  "decltype((c)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((c)))>,
		  "qualification of decltype((c)) is not const");
    static_assert(__is_same (__remove_cvref (decltype((c))), int),
		  "decltype((c)) is not an int");
    static_assert(__is_same (decltype(c), int const));
  };
  f2_value();
  f2_value.template operator()<decltype(f2_value) const>();

  auto f3_value = [&n, &c]<typename Self>(this Self){
    static_assert(__is_same (decltype((n)), int&));
    static_assert(__is_same (decltype(n), int));

    static_assert(__is_same (decltype((c)), int const&));
    static_assert(__is_same (decltype(c), int const));
  };
  f3_value();
  f3_value.template operator()<decltype(f3_value) const>();

  auto f4_value = []<typename Self>(this Self){
    static_assert(__is_same (decltype(n), int));
    static_assert(__is_same (decltype((n)), int&));

    static_assert(__is_same (decltype(c), int const));
    static_assert(__is_same (decltype((c)), int const&));
  };
  f4_value();
  f4_value.template operator()<decltype(f4_value) const>();

  // ref
  auto f0_ref = [=]<typename Self>(this Self&){
    static_assert(is_lvalue_ref<decltype((n))>,
		  "decltype((n)) is not an lvalue ref");
    static_assert(is_const_v<Self> == is_const_v<__remove_reference (decltype((n)))>, // { dg-bogus {static assertion failed: qualification of decltype\(\(n\)\) does not match qualification of Self} }
		  "qualification of decltype((n)) does not match qualification of Self");
    static_assert(__is_same (__remove_cvref (decltype((n))), int),
		  "decltype((n)) is not an int");
    static_assert(__is_same (decltype(n), int));

    static_assert(is_lvalue_ref<decltype((c))>,
		  "decltype((c)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((c)))>,
		  "qualification of decltype((c)) is not const");
    static_assert(__is_same (__remove_cvref (decltype((c))), int),
		  "decltype((c)) is not an int");
    static_assert(__is_same (decltype(c), int const));
  };
  f0_ref();
  f0_ref.template operator()<decltype(f0_ref) const>();
  static_cast<decltype(f0_ref) const&>(f0_ref)();

  auto f1_ref = [&]<typename Self>(this Self&){
    static_assert(__is_same (decltype((n)), int&));
    static_assert(__is_same (decltype(n), int));

    static_assert(__is_same (decltype((c)), int const&));
    static_assert(__is_same (decltype(c), int const));
  };
  f1_ref();
  f1_ref.template operator()<decltype(f1_ref) const>();
  static_cast<decltype(f1_ref) const&>(f1_ref)();

  auto f2_ref = [n, c]<typename Self>(this Self&){
    static_assert(is_lvalue_ref<decltype((n))>,
		  "decltype((n)) is not an lvalue ref");
    static_assert(is_const_v<Self> == is_const_v<__remove_reference (decltype((n)))>, // { dg-bogus {static assertion failed: qualification of decltype\(\(n\)\) does not match qualification of Self} }
		  "qualification of decltype((n)) does not match qualification of Self");
    static_assert(__is_same (__remove_cvref (decltype((n))), int),
		  "decltype((n)) is not an int");
    static_assert(__is_same (decltype(n), int));

    static_assert(is_lvalue_ref<decltype((c))>,
		  "decltype((c)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((c)))>,
		  "qualification of decltype((c)) is not const");
    static_assert(__is_same (__remove_cvref (decltype((c))), int),
		  "decltype((c)) is not an int");
    static_assert(__is_same (decltype(c), int const));
  };
  f2_ref();
  f2_ref.template operator()<decltype(f2_ref) const>();
  static_cast<decltype(f2_ref) const&>(f2_ref)();

  auto f3_ref = [&n, &c]<typename Self>(this Self&){
    static_assert(__is_same(decltype((n)), int&));
    static_assert(__is_same (decltype(n), int));

    static_assert(__is_same (decltype((c)), int const&));
    static_assert(__is_same (decltype(c), int const));
  };
  f3_ref();
  f3_ref.template operator()<decltype(f3_ref) const>();
  static_cast<decltype(f3_ref) const&>(f3_ref)();

  auto f4_ref = []<typename Self>(this Self&){
    static_assert(__is_same (decltype(n), int));
    static_assert(__is_same (decltype((n)), int&));

    static_assert(__is_same (decltype(c), int const));
    static_assert(__is_same (decltype((c)), int const&));
  };
  f4_ref();
  f4_ref.template operator()<decltype(f4_ref) const>();
  static_cast<decltype(f4_ref) const&>(f4_ref)();

  // const value
  auto f0_const_value = [=]<typename Self>(this Self const){
    static_assert(is_lvalue_ref<decltype((n))>,
		  "decltype((n)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((n)))>, // { dg-bogus {static assertion failed: qualification of decltype\(\(n\)\) does not match qualification of Self} }
		  "qualification of decltype((n)) does not match qualification of Self");
    static_assert(__is_same (__remove_cvref (decltype((n))), int),
		  "decltype((n)) is not an int");
    static_assert(__is_same (decltype(n), int));

    static_assert(is_lvalue_ref<decltype((c))>,
		  "decltype((c)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((c)))>,
		  "qualification of decltype((c)) is not const");
    static_assert(__is_same (__remove_cvref (decltype((c))), int),
		  "decltype((c)) is not an int");
    static_assert(__is_same (decltype(c), int const));
  };
  f0_const_value();

  auto f1_const_value = [&]<typename Self>(this Self const){
    static_assert(__is_same (decltype((n)), int&));
    static_assert(__is_same (decltype(n), int));

    static_assert(__is_same (decltype((c)), int const&));
    static_assert(__is_same (decltype(c), int const));
  };
  f1_const_value();

  auto f2_const_value = [n, c]<typename Self>(this Self const){
    static_assert(is_lvalue_ref<decltype((n))>,
		  "decltype((n)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((n)))>, // { dg-bogus {static assertion failed: qualification of decltype\(\(n\)\) does not match qualification of Self} }
		  "qualification of decltype((n)) does not match qualification of Self");
    static_assert(__is_same (__remove_cvref (decltype((n))), int),
		  "decltype((n)) is not an int");
    static_assert(__is_same (decltype(n), int));

    static_assert(is_lvalue_ref<decltype((c))>,
		  "decltype((c)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((c)))>,
		  "qualification of decltype((c)) is not const");
    static_assert(__is_same (__remove_cvref (decltype((c))), int),
		  "decltype((c)) is not an int");
    static_assert(__is_same (decltype(c), int const));
  };
  f2_const_value();

  auto f3_const_value = [&n, &c]<typename Self>(this Self const){
    static_assert(__is_same (decltype((n)), int&));
    static_assert(__is_same (decltype(n), int));

    static_assert(__is_same (decltype((c)), int const&));
    static_assert(__is_same (decltype(c), int const));
  };
  f3_const_value();

  auto f4_const_value = []<typename Self>(this Self const){
    static_assert(__is_same (decltype(n), int));
    static_assert(__is_same (decltype((n)), int&));

    static_assert(__is_same (decltype(c), int const));
    static_assert(__is_same (decltype((c)), int const&));
  };
  f4_const_value();

  // const ref
  auto f0_const_ref = [=]<typename Self>(this Self const&){
    static_assert(is_lvalue_ref<decltype((n))>,
		  "decltype((n)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((n)))>, // { dg-bogus {static assertion failed: qualification of decltype\(\(n\)\) does not match qualification of Self} }
		  "qualification of decltype((n)) does not match qualification of Self");
    static_assert(__is_same (__remove_cvref (decltype((n))), int),
		  "decltype((n)) is not an int");
    static_assert(__is_same (decltype(n), int));

    static_assert(is_lvalue_ref<decltype((c))>,
		  "decltype((c)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((c)))>,
		  "qualification of decltype((c)) is not const");
    static_assert(__is_same (__remove_cvref (decltype((c))), int),
		  "decltype((c)) is not an int");
    static_assert(__is_same (decltype(c), int const));
  };
  f0_const_ref();

  auto f1_const_ref = [&]<typename Self>(this Self const&){
    static_assert(__is_same (decltype((n)), int&));
    static_assert(__is_same (decltype(n), int));

    static_assert(__is_same (decltype((c)), int const&));
    static_assert(__is_same (decltype(c), int const));
  };
  f1_const_ref();

  auto f2_const_ref = [n, c]<typename Self>(this Self const&){
    static_assert(is_lvalue_ref<decltype((n))>,
		  "decltype((n)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((n)))>, // { dg-bogus {static assertion failed: qualification of decltype\(\(n\)\) does not match qualification of Self} }
		  "qualification of decltype((n)) does not match qualification of Self");
    static_assert(__is_same (__remove_cvref (decltype((n))), int),
		  "decltype((n)) is not an int");
    static_assert(__is_same (decltype(n), int));

    static_assert(is_lvalue_ref<decltype((c))>,
		  "decltype((c)) is not an lvalue ref");
    static_assert(is_const_v<__remove_reference (decltype((c)))>,
		  "qualification of decltype((c)) is not const");
    static_assert(__is_same (__remove_cvref (decltype((c))), int),
		  "decltype((c)) is not an int");
    static_assert(__is_same (decltype(c), int const));
  };
  f2_const_ref();

  auto f3_const_ref = [&n, &c]<typename Self>(this Self const&){
    static_assert(__is_same (decltype((n)), int&));
    static_assert(__is_same (decltype(n), int));

    static_assert(__is_same (decltype((c)), int const&));
    static_assert(__is_same (decltype(c), int const));
  };
  f3_const_ref();

  auto f4_const_ref = []<typename Self>(this Self const&){
    static_assert(__is_same (decltype(n), int));
    static_assert(__is_same (decltype((n)), int&));

    static_assert(__is_same (decltype(c), int const));
    static_assert(__is_same (decltype((c)), int const&));
  };
  f4_const_ref();
}

void instantiate_dep()
{
  dep0();
  dep1();
}

