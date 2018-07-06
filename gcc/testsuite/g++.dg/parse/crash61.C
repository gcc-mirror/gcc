// PR c++/56241
// { dg-do compile }
// { dg-additional-options "-Wno-return-type" }

struct pair { constexpr pair (const) : }; // { dg-error "" }
template <0> make_pair () {}		  // { dg-error "" }
pair prefix[] = { 0, make_pair }	  // { dg-error "" }
