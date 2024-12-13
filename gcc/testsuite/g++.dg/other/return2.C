void f(long);
void f(char);

void g()
{
  return f(42);			// { dg-error "ambiguous" }
}				// { dg-bogus "1: void" "" { target *-*-* } .-1 }
