// P1949R7
// { dg-do compile }
// { dg-options "-pedantic-errors" }

bool 👷 = true;	// { dg-error "is not valid in an identifier" "" { target { c++98_only || c++23 } } }
bool 👷‍♀ = false;	// { dg-error "is not valid in an identifier" }
int ⏰ = 0;	// { dg-error "is not valid in an identifier" }
int 🕐 = 0;	// { dg-error "is not valid in an identifier" "" { target { c++98_only || c++23 } } }
int ☠ = 0;	// { dg-error "is not valid in an identifier" }
int 💀 = 0;	// { dg-error "is not valid in an identifier" "" { target { c++98_only || c++23 } } }
int ✋ = 0;	// { dg-error "is not valid in an identifier" }
int 👊 = 0;	// { dg-error "is not valid in an identifier" "" { target { c++98_only || c++23 } } }
int ✈ = 0;	// { dg-error "is not valid in an identifier" }
int 🚀 = 0;	// { dg-error "is not valid in an identifier" "" { target { c++98_only || c++23 } } }
int ☹ = 0;	// { dg-error "is not valid in an identifier" }
int 😀 = 0;	// { dg-error "is not valid in an identifier" "" { target { c++98_only || c++23 } } }
struct E {};
class 💩 : public E {};	// { dg-error "is not valid in an identifier" "" { target { c++98_only || c++23 } } }
