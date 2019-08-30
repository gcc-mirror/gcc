// PR c++/85659
// { dg-do compile }

struct S { S (); ~S (); int s[64]; } s;

void
foo ()
{
  __asm volatile ("" : "=r" (s) : : "memory");	// { dg-error "impossible constraint" }
						// { dg-error "must stay in memory" "" { target *-*-* } .-1 }
}
