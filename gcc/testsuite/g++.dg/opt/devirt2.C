// { dg-do compile }
// { dg-options "-O2" }
// The IA64 and HPPA compilers generate external declarations in addition
// to the call so those scans need to be more specific.
// { dg-final { scan-assembler-times "xyzzy" 2 { target { ! { hppa*-*-* ia64*-*-hpux* sparc*-*-* } } } } }
// { dg-final { scan-assembler-times "br\[^\n\]*xyzzy" 2 { target ia64*-*-hpux* } } }
// { dg-final { scan-assembler-times "xyzzy\[^\n\]*,%r" 2 { target hppa*-*-* } } }
// Unless the assembler supports -relax, the 32-bit SPARC compiler generates
// sethi/jmp instead of just call, so the scans need to be more specific.
// With subexpressions, Tcl regexp -inline -all returns both the complete
// match and the subexpressions, so double the count.
// { dg-final { scan-assembler-times "\(jmp|call\)\[^\n\]*xyzzy" 4 { target sparc*-*-* } } }

struct S { S(); virtual void xyzzy(); };
struct R { int a; S s; R(); };
S s;
R r;
inline void foo(S *p) { p->xyzzy(); }
void bar() {foo(&s);}
void bah() {foo(&r.s);}
