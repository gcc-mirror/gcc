// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test comparison of reflections.  Invalid uses.

consteval void
f ()
{
  bool b = ^^int > ^^int;   // { dg-error "invalid operands of types .std::meta::info. and .std::meta::info. to binary" }
  b = ^^int >= ^^int;	    // { dg-error "invalid operands of types .std::meta::info. and .std::meta::info. to binary" }
  b = ^^int <= ^^int;	    // { dg-error "invalid operands of types .std::meta::info. and .std::meta::info. to binary" }
  +^^int;		    // { dg-error "wrong type argument to unary plus" }
  !^^int;		    // { dg-error "could not convert .\\^\\^int. from .std::meta::info. to .bool." }
			    // { dg-error "in argument to unary" "" { target *-*-* } .-1 }
  ~^^int;		    // { dg-error "wrong type argument to bit-complement" }
  *^^int;		    // { dg-error "invalid type argument of unary" }
}
