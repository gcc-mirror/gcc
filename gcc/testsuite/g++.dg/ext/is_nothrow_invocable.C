// { dg-do compile { target c++11 } }

#define SA(X) static_assert((X),#X)

using func_type = void(*)();
SA( ! __is_nothrow_invocable(func_type) );

#if __cpp_noexcept_function_type
using func_type_nt = void(*)() noexcept;
SA(   __is_nothrow_invocable(func_type_nt) );
#endif

struct X { };
using mem_type = int X::*;

SA( ! __is_nothrow_invocable(mem_type) );
SA( ! __is_nothrow_invocable(mem_type, int) );
SA( ! __is_nothrow_invocable(mem_type, int&) );
SA(   __is_nothrow_invocable(mem_type, X&) );

using memfun_type = int (X::*)();

SA( ! __is_nothrow_invocable(memfun_type) );
SA( ! __is_nothrow_invocable(memfun_type, int) );
SA( ! __is_nothrow_invocable(memfun_type, int&) );
SA( ! __is_nothrow_invocable(memfun_type, X&) );
SA( ! __is_nothrow_invocable(memfun_type, X*) );

#if __cpp_noexcept_function_type
using memfun_type_nt = int (X::*)() noexcept;

SA( ! __is_nothrow_invocable(memfun_type_nt) );
SA( ! __is_nothrow_invocable(memfun_type_nt, int) );
SA( ! __is_nothrow_invocable(memfun_type_nt, int&) );
SA(   __is_nothrow_invocable(memfun_type_nt, X&) );
SA(   __is_nothrow_invocable(memfun_type_nt, X*) );
#endif

struct F {
  int& operator()();
  long& operator()() const noexcept;
  short& operator()(int) &&;
  char& operator()(int) const& noexcept;
private:
  void operator()(int, int) noexcept;
};
using CF = const F;

SA( ! __is_nothrow_invocable(F ) );
SA(   __is_nothrow_invocable(CF) );

SA( ! __is_nothrow_invocable(F,   int) );
SA(   __is_nothrow_invocable(F&,  int) );

SA(   __is_nothrow_invocable(CF,   int) );
SA(   __is_nothrow_invocable(CF&,  int) );
SA( ! __is_nothrow_invocable(F, int, int) );

struct FX {
  X operator()() const noexcept { return {}; }
};
SA(   __is_nothrow_invocable(FX) );
