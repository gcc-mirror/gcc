// PR c++/89325
// { dg-do compile }
// { dg-options "-Wattributes" }

__attribute__((noinline)) void foo (int) {}	// { dg-bogus "previous definition" } 
inline void foo (long);				// { dg-bogus "inline declaration of '\[^\n\r]*' follows declaration with attribute 'noinline'" }
inline void foo (long) {}
__attribute__((noinline)) void bar (int) {}	// { dg-message "previous definition" } 
inline void bar (int);				// { dg-warning "inline declaration of '\[^\n\r]*' follows declaration with attribute 'noinline'" }
