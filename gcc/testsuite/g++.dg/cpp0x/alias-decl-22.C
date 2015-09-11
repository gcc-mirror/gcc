// The standard is unclear about whether this testcase is well-formed.
// Clang considers it well-formed, EDG not.  Let's go with EDG for now.
// { dg-do compile { target c++11 } }

template <class T>
using foo = typename T::bar;	// { dg-error "this context" }

class B
{
  typedef int bar;		// { dg-message "private" }
  foo<B> f;			// { dg-message "required" }
};
