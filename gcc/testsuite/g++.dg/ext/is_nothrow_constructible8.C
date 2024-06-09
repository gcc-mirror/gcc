// { dg-do compile { target c++14 } }
// PR c++/96090

template <typename T>
constexpr bool is_nothrow_default_constructible_v
  = __is_nothrow_constructible(T);
template <typename T>
constexpr bool is_nothrow_copy_constructible_v
  = __is_nothrow_constructible(T, const T&);
template <typename T>
constexpr bool is_nothrow_move_constructible_v
  = __is_nothrow_constructible(T, T&&);
template <typename T>
constexpr bool is_nothrow_copy_assignable_v
  = __is_nothrow_assignable(T, const T&);
template <typename T>
constexpr bool is_nothrow_move_assignable_v
  = __is_nothrow_assignable(T, T&&);

struct yesthrow_t {
  yesthrow_t()                              noexcept(false) = default;
  yesthrow_t(const yesthrow_t&)             noexcept(false) = default;
  yesthrow_t(yesthrow_t&&)                  noexcept(false) = default;
  yesthrow_t& operator=(const yesthrow_t&)  noexcept(false) = default;
  yesthrow_t& operator=(yesthrow_t&&)       noexcept(false) = default;
};

static_assert(not is_nothrow_copy_constructible_v<yesthrow_t>, "");
static_assert(not is_nothrow_copy_assignable_v<yesthrow_t>, "");
static_assert(not is_nothrow_move_constructible_v<yesthrow_t>, "");
static_assert(not is_nothrow_move_assignable_v<yesthrow_t>, "");

// Note: by [meta.unary.prop] p9 this should be value-initialisation,
// and thus by [dcl.init.general] p9 a trivial non-user-provided
// non-deleted default constructor is not called.
// However, CWG2820 proposes to change this behaviour.
static_assert(is_nothrow_default_constructible_v<yesthrow_t>, "");

struct nothrow_t {
  nothrow_t()                             noexcept(true) = default;
  nothrow_t(const nothrow_t&)             noexcept(true) = default;
  nothrow_t(nothrow_t&&)                  noexcept(true) = default;
  nothrow_t& operator=(const nothrow_t&)  noexcept(true) = default;
  nothrow_t& operator=(nothrow_t&&)       noexcept(true) = default;
};

static_assert(is_nothrow_default_constructible_v<nothrow_t>, "");
static_assert(is_nothrow_copy_constructible_v<nothrow_t>, "");
static_assert(is_nothrow_copy_assignable_v<nothrow_t>, "");
static_assert(is_nothrow_move_constructible_v<nothrow_t>, "");
static_assert(is_nothrow_move_assignable_v<nothrow_t>, "");

struct A { A() noexcept(false) = default; };
struct B { B(const B&) noexcept(false) = default; };
struct C { C(C&&) noexcept(false) = default; };
struct D { D& operator=(const D&) noexcept(false) = default; };
struct E { E& operator=(E&&) noexcept(false) = default; };

static_assert(is_nothrow_default_constructible_v<A>, "");  // see above
static_assert(not is_nothrow_copy_constructible_v<B>, "");
static_assert(not is_nothrow_move_constructible_v<C>, "");
static_assert(not is_nothrow_copy_assignable_v<D>, "");
static_assert(not is_nothrow_move_assignable_v<E>, "");

