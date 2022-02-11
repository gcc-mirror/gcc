// PR c++/103408
// { dg-do compile { target c++23 } }

static_assert(requires { auto(0); });
static_assert(requires { auto{0}; });

static_assert(requires { auto(auto(0)); });
static_assert(requires { auto{auto{0}}; });

static_assert(requires { auto(auto(auto(0))); });
static_assert(requires { auto{auto{auto{0}}}; });

static_assert(requires { requires auto(true); });
static_assert(requires { requires auto(auto(true)); });

static_assert(!requires { requires auto(false); });
static_assert(!requires { requires auto(auto(false)); });

auto f() requires (auto(false)); // { dg-error "constraints on a non-templated" }
