// { dg-do compile }
// { dg-options "-O2" }
/* Using -mshort-calls avoids loading the function addresses in
   registers and thus getting the counts wrong.  */
// { dg-additional-options "-mshort-calls" {target epiphany-*-*} }
// Using -mno-abicalls avoids a R_MIPS_JALR .reloc.
// { dg-additional-options "-mno-abicalls" { target mips*-*-* } }
// { dg-final { scan-assembler-times "xyzzy" 2 { target { ! { alpha*-*-* hppa*-*-* ia64*-*-hpux* sparc*-*-* *-*-mingw* } } } } }
// For *-*-mingw* there is additionally one .def match
// { dg-final { scan-assembler-times "xyzzy" 3 { target *-*-mingw* } } }
// The IA64 and HPPA compilers generate external declarations in addition
// to the call so those scans need to be more specific.
// { dg-final { scan-assembler-times "br\[^\n\]*xyzzy" 2 { target ia64*-*-hpux* } } }
// { dg-final { scan-assembler-times "xyzzy\[^\n\]*,%r" 2 { target hppa*-*-* } } }
// If assembler supports explicit relocation directives, the alpha compiler generates
// literal/lituse_jsr pairs, so the scans need to be more specific.
// { dg-final { scan-assembler-times "jsr\[^\n\]*xyzzy" 2 { target alpha*-*-* } } }
// Unless the assembler supports -relax, the 32-bit SPARC compiler generates
// sethi/jmp instead of just call, so the scans need to be more specific.
// { dg-final { scan-assembler-times "\(jmp|call\)\[^\n\]*xyzzy" 2 { target sparc*-*-* } } }

struct S { S(); virtual void xyzzy(); };
struct R { int a; S s; R(); };
S s;
R r;
inline void foo(S *p) { p->xyzzy(); }
void bar() {foo(&s);}
void bah() {foo(&r.s);}
