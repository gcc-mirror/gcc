// Build don't link:
// From: Klaus-Georg Adams <Klaus-Georg.Adams@chemie.uni-karlsruhe.de> 
// Reported against EGCS snaps 98/06/28.
// Special g++ Options: -O -Wall  -fgcse -frerun-loop-opt
//
// Compilation of this program with the flags g++ -Wall -O -fgcse
// -frerun-loop-opt or -O2 produces spurious warnings in the standard
// header <std/bastring.h>.
//
// They vanish if the declaration of a::b is taken out.

#include <string>

string foo();
struct a {
	void bar();
	enum b { c, d };
	b theb;
	string baz;
};

void
a::bar()
{
	baz += foo() + foo();
	baz += foo() + foo() + "foo";
}

