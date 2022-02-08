// PR c++/80951
// { dg-do compile { target c++17 } }

void f() noexcept;
void g();

template<bool E>
constexpr bool h(void (*)() noexcept(E)) {
  return E;
}

static_assert(h(f));
static_assert(!h(g));
