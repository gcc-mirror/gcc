// PR c++/77369
// { dg-do compile { target c++11 } }

template<typename F> int caller(F f) noexcept(noexcept(f())) { f(); return 0; }

void func1() noexcept { }

void func2() { throw 1; }

int instantiate_caller_with_func1 = caller(func1);

static_assert( !noexcept(caller(func2)), "" );
