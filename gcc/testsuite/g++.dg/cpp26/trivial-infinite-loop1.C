// P2809R3 - Trivial infinite loops are not Undefined Behavior
// { dg-do compile { target c++11 } }
// { dg-additional-options "-fdump-tree-gimple -fno-inline -Wtautological-compare -O2" }
// { dg-final { scan-tree-dump-times ".ANNOTATE \\\(\[^\n\r]*, 5, 0\\\)" 32 "gimple" { target c++20 } } }
// { dg-final { scan-tree-dump-times ".ANNOTATE \\\(\[^\n\r]*, 5, 0\\\)" 16 "gimple" { target c++17_down } } }

volatile int v;

constexpr bool
foo ()
{
  return true;
}

struct S
{
  constexpr S () : s (true) {}
  constexpr operator bool () const { return s; }
  bool s;
};

#if __cplusplus >= 202002L
namespace std {
  constexpr inline bool
  is_constant_evaluated () noexcept
  {
#if __cpp_if_consteval >= 202106L
    if consteval { return true; } else { return false; }
#else
    return __builtin_is_constant_evaluated ();
#endif
  }
}

constexpr bool
baz ()
{
  return std::is_constant_evaluated ();
}
#endif

void
bar (int x)
{
  switch (x)
    {
    case 0:
      while (foo ()) ;
      break;
    case 1:
      while (foo ()) {}
      break;
    case 2:
      do ; while (foo ());
      break;
    case 3:
      do {} while (foo ());
      break;
    case 4:
      for (v = 42; foo (); ) ;
      break;
    case 5:
      for (v = 42; foo (); ) {}
      break;
    case 6:
      for (int w = 42; foo (); ) ;
      break;
    case 7:
      for (int w = 42; foo (); ) {}
      break;
    case 10:
      while (S {}) ;
      break;
    case 11:
      while (S {}) {}
      break;
    case 12:
      do ; while (S {});
      break;
    case 13:
      do {} while (S {});
      break;
    case 14:
      for (v = 42; S {}; ) ;
      break;
    case 15:
      for (v = 42; S {}; ) {}
      break;
    case 16:
      for (int w = 42; S {}; ) ;
      break;
    case 17:
      for (int w = 42; S {}; ) {}
      break;
#if __cplusplus >= 202002L
    case 20:
      while (baz ()) ;
      break;
    case 21:
      while (baz ()) {}
      break;
    case 22:
      do ; while (baz ());
      break;
    case 23:
      do {} while (baz ());
      break;
    case 24:
      for (v = 42; baz (); ) ;
      break;
    case 25:
      for (v = 42; baz (); ) {}
      break;
    case 26:
      for (int w = 42; baz (); ) ;
      break;
    case 27:
      for (int w = 42; baz (); ) {}
      break;
    case 30:
      while (std::is_constant_evaluated ()) ;			// { dg-warning "'std::is_constant_evaluated' evaluates to true when checking if trivially empty iteration statement is trivial infinite loop" "" { target c++20 } }
      break;							// { dg-message "and evaluates to false when actually evaluating the condition in non-'constexpr' function" "" { target c++20 } .-1 }
    case 31:
      while (std::is_constant_evaluated ()) {}			// { dg-warning "'std::is_constant_evaluated' evaluates to true when checking if trivially empty iteration statement is trivial infinite loop" "" { target c++20 } }
      break;							// { dg-message "and evaluates to false when actually evaluating the condition in non-'constexpr' function" "" { target c++20 } .-1 }
    case 32:
      do ; while (std::is_constant_evaluated ());		// { dg-warning "'std::is_constant_evaluated' evaluates to true when checking if trivially empty iteration statement is trivial infinite loop" "" { target c++20 } }
      break;							// { dg-message "and evaluates to false when actually evaluating the condition in non-'constexpr' function" "" { target c++20 } .-1 }
    case 33:
      do {} while (std::is_constant_evaluated ());		// { dg-warning "'std::is_constant_evaluated' evaluates to true when checking if trivially empty iteration statement is trivial infinite loop" "" { target c++20 } }
      break;							// { dg-message "and evaluates to false when actually evaluating the condition in non-'constexpr' function" "" { target c++20 } .-1 }
    case 34:
      for (v = 42; std::is_constant_evaluated (); ) ;		// { dg-warning "'std::is_constant_evaluated' evaluates to true when checking if trivially empty iteration statement is trivial infinite loop" "" { target c++20 } }
      break;							// { dg-message "and evaluates to false when actually evaluating the condition in non-'constexpr' function" "" { target c++20 } .-1 }
    case 35:
      for (v = 42; std::is_constant_evaluated (); ) {}		// { dg-warning "'std::is_constant_evaluated' evaluates to true when checking if trivially empty iteration statement is trivial infinite loop" "" { target c++20 } }
      break;							// { dg-message "and evaluates to false when actually evaluating the condition in non-'constexpr' function" "" { target c++20 } .-1 }
    case 36:
      for (int w = 42; std::is_constant_evaluated (); ) ;	// { dg-warning "'std::is_constant_evaluated' evaluates to true when checking if trivially empty iteration statement is trivial infinite loop" "" { target c++20 } }
      break;							// { dg-message "and evaluates to false when actually evaluating the condition in non-'constexpr' function" "" { target c++20 } .-1 }
    case 37:
      for (int w = 42; std::is_constant_evaluated (); ) {}	// { dg-warning "'std::is_constant_evaluated' evaluates to true when checking if trivially empty iteration statement is trivial infinite loop" "" { target c++20 } }
      break;							// { dg-message "and evaluates to false when actually evaluating the condition in non-'constexpr' function" "" { target c++20 } .-1 }
#endif
    default:
      break;
    }
}
