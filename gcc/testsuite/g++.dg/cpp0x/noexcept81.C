// { dg-do compile { target c++11 } }
// PR c++/96090

struct yesthrow_t {
  yesthrow_t()                              noexcept(false) = default;
  yesthrow_t(const yesthrow_t&)             noexcept(false) = default;
  yesthrow_t(yesthrow_t&&)                  noexcept(false) = default;
  yesthrow_t& operator=(const yesthrow_t&)  noexcept(false) = default;
  yesthrow_t& operator=(yesthrow_t&&)       noexcept(false) = default;
};

yesthrow_t yes;
static_assert(not noexcept(yesthrow_t(static_cast<const yesthrow_t&>(yes))), "");
static_assert(not noexcept(yesthrow_t(static_cast<yesthrow_t&&>(yes))), "");
static_assert(not noexcept(yes = static_cast<const yesthrow_t&>(yes)), "");
static_assert(not noexcept(yes = static_cast<yesthrow_t&&>(yes)), "");

// Note: this is value-initialisation, and thus by [dcl.init.general] p9
// a trivial non-user-provided non-deleted default constructor is not called.
// However, CWG2820 proposes to change this behaviour.
static_assert(noexcept(yesthrow_t()), "");

struct nothrow_t {
  nothrow_t()                             noexcept(true) = default;
  nothrow_t(const nothrow_t&)             noexcept(true) = default;
  nothrow_t(nothrow_t&&)                  noexcept(true) = default;
  nothrow_t& operator=(const nothrow_t&)  noexcept(true) = default;
  nothrow_t& operator=(nothrow_t&&)       noexcept(true) = default;
};

nothrow_t no;
static_assert(noexcept(nothrow_t()), "");
static_assert(noexcept(nothrow_t(static_cast<const nothrow_t&>(no))), "");
static_assert(noexcept(nothrow_t(static_cast<nothrow_t&&>(no))), "");
static_assert(noexcept(no = static_cast<const nothrow_t&>(no)), "");
static_assert(noexcept(no = static_cast<nothrow_t&&>(no)), "");

