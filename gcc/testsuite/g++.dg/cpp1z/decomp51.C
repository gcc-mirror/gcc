// PR c++/94571
// { dg-do compile { target c++17 } }

void
foo ()
{
  int e[2], f[2];
  auto [a,b] = e, [c,d] = f;	// { dg-error "expected ';' before ',' token" }
}

void
bar ()
{
  int e[2];
  auto [a, b] = e );		// { dg-error "expected ';' before '\\\)' token" }
}
