// { dg-do assemble  }
// { dg-options "-O -Wall  -fgcse" }
// { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } }
// From: Klaus-Georg Adams <Klaus-Georg.Adams@chemie.uni-karlsruhe.de> 
// Reported against EGCS snaps 98/06/28.
//
// Compilation of this program with the flags g++ -Wall -O -fgcse
// or -O2 produces spurious warnings in the standard
// header <std/bastring.h>.
//
// They vanish if the declaration of a::b is taken out.

#include <string>

std::string foo();
struct a {
	void bar();
	enum b { c, d };
	b theb;
	std::string baz;
};

void
a::bar()
{
	baz += foo() + foo();
	baz += foo() + foo() + "foo";
}

