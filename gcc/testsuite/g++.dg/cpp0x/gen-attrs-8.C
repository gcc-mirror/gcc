// { dg-options "-std=c++11 -pedantic" }
// { dg-do compile { target { { i?86-*-* x86_64-*-* } && ia32 } } }

extern int * ([[gnu::stdcall]] *fooPtr)( void); // { dg-error "expected" }
int * [[gnu::stdcall]] myFn01( void) { return 0; }// { dg-warning "ignored" }

