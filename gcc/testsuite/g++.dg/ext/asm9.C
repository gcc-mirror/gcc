// PR 27451
// { dg-do compile }

void foo()
{
  asm("" ::: X); // { dg-error "string-literal" }
		 // { dg-error "before" "" { target *-*-* } .-1 }
}
