// { dg-do compile { target c++2a } }

template<typename T>
concept True = true;

template<typename T>
concept False = false;

template<typename T>
concept Int = __is_same_as(T, int);

static_assert(True<int>);
static_assert(!False<int>);
static_assert(False<int>); // { dg-error "static assertion failed" }

constexpr bool will_be_true() {
  if (True<int>)
    return true;
  return false;
}

constexpr bool will_be_false() {
  if (!False<int>)
    return true;
  return false;
}

static_assert(will_be_true());
static_assert(will_be_false());

template<typename T>
constexpr bool is_int() {
  if (Int<T>)
    return true;
  return false;
}

static_assert(is_int<int>());
static_assert(is_int<void>()); // { dg-error "static assertion failed" }

template<typename T>
constexpr bool f1() {
  if (Int<int>) //  Note: always true.
    return true;
  return false;
}
static_assert(f1<int>());
static_assert(f1<void>());


template<typename T>
constexpr bool f2() {
  if constexpr (Int<int>) // Note: always true.
    return true;
  return false;
}
static_assert(f2<int>());
static_assert(f2<void>());

template<typename T>
concept C = true;

int driver() {
  bool c_int = (C<int>); 
  if((C<int>))
    ;
  return c_int;
}

