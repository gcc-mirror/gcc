// PR c++/59633
// In C++98, the definition of bar is an error.  In C++11, bar implicitly
// gets internal linkage.

typedef enum { e } T __attribute__((vector_size(8)));
static void foo(T t) {}
void bar (T t) {}		// { dg-error "no linkage" "" { target c++98 } }
// { dg-final { scan-assembler-not "globl\[ \t]*_Z3bar" { target { ! c++98 } } } }
