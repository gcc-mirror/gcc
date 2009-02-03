// PR c++/35319
// { dg-options "" }

void foo()
{
  throw 0r;	// { dg-error "not supported" }
}
