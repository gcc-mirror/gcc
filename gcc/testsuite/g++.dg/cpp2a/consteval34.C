// { dg-do compile { target c++20 } }
// Explicit { dg-require-effective-target exceptions_enabled } to avoid verify compiler messages FAILs for '-fno-exceptions'.

consteval int bar (int i) { if (i != 1) throw 1; return 0; }	// { dg-error "is not a constant expression" "" { target c++23_down } }

constexpr int
foo (bool b)
{
  return b ? bar (3) : 2; // { dg-message "in .constexpr. expansion" "" { target c++23_down } }
// { dg-error "call to consteval function" "" { target *-*-* } .-1 }
} // { dg-error "uncaught exception '1'" "" { target c++26 } }

static_assert (foo (false) == 2);

__extension__ constexpr int g1 = false ?: bar (2); // { dg-message "in .constexpr. expansion" "" { target c++23_down } }
// { dg-error "uncaught exception '1'" "" { target c++26 } .-1 }
__extension__ constexpr int g2 = false ?: (1 + bar (2)); // { dg-message "in .constexpr. expansion" "" { target c++23_down } }
// { dg-error "uncaught exception '1'" "" { target c++26 } .-1 }
__extension__ constexpr int g3 = true ?: bar (2);
__extension__ constexpr int g4 = true ?: (1 + bar (2));
constexpr int g5 = bar (2) ? 1 : 2; // { dg-message "in .constexpr. expansion" "" { target c++23_down } }
// { dg-error "uncaught exception '1'" "" { target c++26 } .-1 }
constexpr int g6 = bar (2) - 1 ? 1 : 2; // { dg-message "in .constexpr. expansion" "" { target c++23_down } }
// { dg-error "uncaught exception '1'" "" { target c++26 } .-1 }

void
g ()
{
  __extension__ int a1[bar(3)]; // { dg-message "in .constexpr. expansion" "" { target c++23_down } }
// { dg-error "call to consteval function" "" { target *-*-* } .-1 }
  int a2[sizeof (bar(3))];

  int a3 = false ? (1 + bar (8)) : 1; // { dg-message "in .constexpr. expansion" "" { target c++23_down } }
// { dg-error "call to consteval function" "" { target *-*-* } .-1 }
// { dg-error "uncaught exception '1'" "" { target c++26 } .-2 }
  a3 += false ? (1 + bar (8)) : 1; // { dg-message "in .constexpr. expansion" "" { target c++23_down } }
// { dg-error "call to consteval function" "" { target *-*-* } .-1 }

  __extension__ int a4 = false ?: (1 + bar (8)); // { dg-message "in .constexpr. expansion" "" { target c++23_down } }
// { dg-error "call to consteval function" "" { target *-*-* } .-1 }
// { dg-error "uncaught exception '1'" "" { target c++26 } .-2 }
  __extension__ int a5 = true ?: (1 + bar (8)); // { dg-message "in .constexpr. expansion" "" { target c++23_down } }
// { dg-error "call to consteval function" "" { target *-*-* } .-1 }
// { dg-error "uncaught exception '1'" "" { target c++26 } .-2 }
  int a6 = bar (2) ? 1 : 2; // { dg-message "in .constexpr. expansion" "" { target c++23_down } }
// { dg-error "call to consteval function" "" { target *-*-* } .-1 }
// { dg-error "uncaught exception '1'" "" { target c++26 } .-2 }
  int a7 = bar (2) - 1 ? 1 : 2; // { dg-message "in .constexpr. expansion" "" { target c++23_down } }
// { dg-error "call to consteval function" "" { target *-*-* } .-1 }
// { dg-error "uncaught exception '1'" "" { target c++26 } .-2 }
} // { dg-error "uncaught exception '1'" "" { target c++26 } }
