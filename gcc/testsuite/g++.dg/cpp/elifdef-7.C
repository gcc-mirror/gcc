// P2334R1
// { dg-do preprocess }
// { dg-options "-pedantic-errors" }

#define A
#undef B

#if 0
#elifdef A	// { dg-error "#elifdef before C\\\+\\\+23 is a GCC extension" "" { target c++20_down } }
#define M1 1
#endif

#if M1 != 1
#error "#elifdef A did not apply"
#endif

#if 0
#elifdef B
#error "#elifdef B applied"
#endif

#if 0
#elifndef A
#error "#elifndef A applied"
#endif

#if 0
#elifndef B	// { dg-error "#elifndef before C\\\+\\\+23 is a GCC extension" "" { target c++20_down } }
#define M2 2
#endif

#if M2 != 2
#error "#elifndef B did not apply"
#endif

#if 0
#elifdef A	// { dg-error "#elifdef before C\\\+\\\+23 is a GCC extension" "" { target c++20_down } }
#else
#error "#elifdef A did not apply"
#endif

#if 0
#elifndef B	// { dg-error "#elifndef before C\\\+\\\+23 is a GCC extension" "" { target c++20_down } }
#else
#error "#elifndef B did not apply"
#endif

#if 1
#elifdef A	// { dg-error "#elifdef before C\\\+\\\+23 is a GCC extension" "" { target c++20_down } }
#endif

#if 1
#elifndef B	// { dg-error "#elifndef before C\\\+\\\+23 is a GCC extension" "" { target c++20_down } }
#endif

// As with #elif, the syntax of the new directives is relaxed after a
   non-skipped group. 

#if 1
#elifdef x * y	// { dg-error "#elifdef before C\\\+\\\+23 is a GCC extension" "" { target c++20_down } }
#endif

#if 1
#elifndef !	// { dg-error "#elifndef before C\\\+\\\+23 is a GCC extension" "" { target c++20_down } }
#endif
