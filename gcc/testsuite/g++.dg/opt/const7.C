// PR c++/104142
// { dg-do compile { target c++11 } }
// { dg-additional-options -Wunused-variable }

struct B { B()=default; };
static const B b_var;		//  { dg-bogus "" }
// { dg-final { scan-assembler "\\.rodata" { target i?86-*-linux* i?86-*-gnu* x86_64-*-linux* } } }
