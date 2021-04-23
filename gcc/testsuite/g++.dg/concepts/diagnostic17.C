// PR c++/98767
// { dg-do compile { target c++20 } }

template <typename Function, typename... Args>
concept Callable = requires(Function func, Args... args) { func(args...); };

static_assert(Callable<int(*)(), bool>); // { dg-error "failed" }
// { dg-message {Function = int \(\*\)\(\)} "" { target *-*-* } 5 }

static_assert(Callable<char(*)(int*), bool>); // { dg-error "failed" }
// { dg-message {Function = char \(\*\)\(int\*\)} "" { target *-*-* } 5 }

static_assert(Callable<short(*)(int*, int), bool>); // { dg-error "failed" }
// { dg-message {Function = short int \(\*\)\(int\*, int\)} "" { target *-*-* } 5 }

static_assert(Callable<long(*)(int*, int, ...), bool>); // { dg-error "failed" }
// { dg-message {Function = long int \(\*\)\(int\*, int, \.\.\.\)} "" { target *-*-* } 5 }
