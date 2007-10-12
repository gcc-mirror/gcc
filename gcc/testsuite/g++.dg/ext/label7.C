// PR c++/32121
// { dg-do compile }

int f (void)
{
  a:;
  __label__ a;	// { dg-error "not at the beginning" }
  int b;
  __label__ c;	// { dg-error "not at the beginning" }
  a:;		// { dg-error "duplicate label" }
  c:;
}
