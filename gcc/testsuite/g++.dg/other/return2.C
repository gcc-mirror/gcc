void f(long);
void f(char);

void g()
{
  return f(42);			// { dg-error "ambiguous" }
}				// { dg-bogus "void" "" { target *-*-* } .-1 }
