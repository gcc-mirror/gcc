// PR c++/121601
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct A {
  constexpr const char *data () const noexcept { return "abc"; }
  constexpr unsigned size () const noexcept { return 3; }
};

constexpr A
foo ()
{
  return true ? throw 42 : A {}; // { dg-warning "expression '<throw-expression>' is not a constant expression" "" { target c++20_down } }
}				 // { dg-error "expression '<throw-expression>' is not a constant expression" "" { target c++23_only } .-1 }

static_assert (false, foo ());	 // { dg-warning "'static_assert' with non-string message only available with" "" { target c++23_down } }
// { dg-error "'constexpr A foo\\\(\\\)' called in a constant expression" "" { target c++23_down } .-1 }
// { dg-error "constexpr string 'size\\\(\\\)' must be a constant expression" "" { target *-*-* } .-2 }
// { dg-error "uncaught exception '42'" "" { target c++26 } .-3 }
