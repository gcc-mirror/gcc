// PR c++/112436
// { dg-do compile { target c++20 } }

template<class T>
concept Throwable = requires { throw T(); };
struct Incomplete;

static_assert(!Throwable<Incomplete*>);
static_assert(!Throwable<int(*)[]>);
