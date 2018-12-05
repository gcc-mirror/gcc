// PR debug/85302
// { dg-do compile }
// { dg-skip-if "split DWARF unsupported" { *-*-darwin* } }
// { dg-options "-std=c++11 -gsplit-dwarf -O1" }
// { dg-additional-options "-fPIE" { target pie } }

struct A { const char *b; A (const char *c) : b(c) {} };
struct B { void foo (A); };
B e;

void
bar ()
{
  e.foo ("");
}
