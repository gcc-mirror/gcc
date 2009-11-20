// { dg-do assemble  }
// Here is a simple one.  GCC correctly gives errors for this code when the
// - -pedantic-errors option is used, whereas g++ doesn't.


int (*p1)[3];
int (*p2)[5];

void
test ()
{
  p1 == p2;		// { dg-error "comparison between distinct pointer types" } comparison.*
  p1 > p2;		// { dg-error "comparison between distinct pointer types" } comparison.*
}
