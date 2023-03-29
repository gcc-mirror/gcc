// PR c++/105300
// { dg-do compile { target c++11 } }

void operator""_x(const char *, decltype(sizeof(0)));

#include ""_x		  // { dg-error "include expects" }
#line ""_x		  // { dg-error "not a positive integer" }
#if __has_include(""_x)	  // { dg-error "requires a header-name" }
#endif

#pragma message "hi"_x	  // { dg-warning "string literal with user-defined suffix is invalid in this context" }

extern "C"_x { void g(); } // { dg-error "before user-defined string literal" }
static_assert(true, "foo"_x); // { dg-error "string literal with user-defined suffix is invalid in this context|expected" }

[[deprecated("oof"_x)]]
void
lol () // { dg-error "not a string" }
{
  asm (""_x); // { dg-error "string literal with user-defined suffix is invalid in this context" }
}
