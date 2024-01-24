//PR c++/30851

void foo()
{
  asm ("%[x]" : [0](x));  // { dg-error "numeric constant|token" }
			  // { dg-error "string-literal" "" { target *-*-* } .-1 }
}
