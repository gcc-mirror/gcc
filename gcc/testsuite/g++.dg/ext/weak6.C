// PR c++/99035
// { dg-do compile }
// { dg-require-weak "" }
// { dg-options "-fsyntax-only" }

extern void * foo (void);
void * foo (void) { return (void *)foo; }
#pragma weak foo
