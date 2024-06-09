// PR c++/105300
// { dg-do compile { target c++11 } }

void operator""_x(const char *, decltype(sizeof(0)));

#include ""_x		  // { dg-error "include expects" }
#line ""_x		  // { dg-error "not a positive integer" }
#if __has_include(""_x)	  // { dg-error "requires a header-name" }
#endif

#pragma message "hi"_x	  // { dg-warning "string literal with user-defined suffix is invalid in this context" }

extern "C"_x { void g(); } // { dg-error "before user-defined string literal" }
static_assert(true, "foo"_x);	// { dg-error "'static_assert' with non-string message only available with" "" { target c++23_down } }
				// { dg-error "'static_assert' message must be a string literal or object with 'size' and 'data' members" "" { target *-*-* } .-1 }
				// { dg-error "invalid use of 'void'" "" { target *-*-* } .-2 }

[[deprecated("oof"_x)]]	// { dg-error "string literal with user-defined suffix is invalid in this context" "" { target c++26 } }
void
lol () // { dg-error "not a string" }
{
  asm (""_x); // { dg-error "string literal with user-defined suffix is invalid in this context" }
}
