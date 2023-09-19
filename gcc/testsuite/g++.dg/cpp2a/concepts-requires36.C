// PR c++/111419
// { dg-do compile { target c++20 } }

template<class F>
concept invocable = requires(F& f) { f(); };

template<class F>
concept deref_invocable = requires(F& f) { *f(); };

struct Incomplete;

template<class T>
struct Holder { T t; };

static_assert(invocable<Holder<Incomplete>& ()>);
static_assert(deref_invocable<Holder<Incomplete>* ()>);
