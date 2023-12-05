// DR 2262 - Attributes for asm-definition
// { dg-do compile { target c++11 } }
// { dg-options "-Wattributes" }

[[]] asm ("nop");
[[foo::bar]] asm ("nop");	// { dg-warning "attributes ignored on 'asm' declaration" }

void
foo ()
{
  int i = 42;
  [[]] asm ("nop");
  [[foo::bar]] asm ("nop");	// { dg-warning "attributes ignored on 'asm' declaration" }
  [[]] asm ("nop" : "+r" (i));
  [[foo::bar]] [[bar::baz]] asm ("nop" : "+r" (i));	// { dg-warning "attributes ignored on 'asm' declaration" }
}
