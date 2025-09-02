// { dg-do compile { target c++26 } }
// Test consteval blocks, as specified by P2996.

/* __func__ won't be set.  Make sure we warn.  */
consteval { __func__; }	  // { dg-error "outside of function scope" }
consteval { { __func__; } }	  // { dg-error "outside of function scope" }
consteval { []() mutable consteval -> void { __func__; } (); }	// { dg-bogus "outside of function scope" }
consteval { []() mutable consteval -> void { consteval { __func__; } } (); } // { dg-bogus "outside of function scope" }

auto l = []() -> void {
  consteval { __func__; } // { dg-bogus "outside of function scope" }
};

struct F {
  consteval { __func__; } // { dg-error "outside of function scope" }
};
template<typename>
struct TF {
  consteval { __func__; } // { dg-error "outside of function scope" }
};

void
g ()
{
  consteval { __func__; } // { dg-bogus "outside of function scope" }
  // Not a consteval-block-declaration.
  []() mutable consteval -> void { __func__; } (); // { dg-bogus "outside of function scope" }
}

template<typename>
void
f ()
{
  consteval { __func__; } // { dg-bogus "outside of function scope" }
  { consteval { __func__; } } // { dg-bogus "outside of function scope" }
  __func__; // { dg-bogus "outside of function scope" }
  []() mutable consteval -> void { __func__; } (); // { dg-bogus "outside of function scope" }
}
