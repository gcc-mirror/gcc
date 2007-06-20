// PR inline-asm/32109
// { dg-do compile }
// { dg-options "-O2" }

struct A { int i[3]; ~A (); };
struct A a;
struct B { struct A c; int i; B (); } b;

B::B ()
{
  __asm ("" : : "r" (a));	// { dg-error "impossible constraint|non-memory input" }
  __asm ("" : : "r" (b.c));	// { dg-error "impossible constraint|non-memory input" }
  __asm ("" : : "r" (c));	// { dg-error "impossible constraint|non-memory input" }
}
