// Test that the second foo is not considered more specialized because we don't
// compare constraints unless the template parameters and function parameters
// are equivalent (P2113)

// { dg-do compile { target c++20 } }

template <typename T> concept P = true;

template <typename T> void foo(int, T);
template <P U>        void foo(U, int);

void bar() { foo(1,2); }	// { dg-error "ambiguous" }
