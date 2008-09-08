
// Test to make sure we do not ICE on this invalid program.

template<int> void foo(struct {}*); // { dg-error "" }

void bar()
{
  foo<0>(0);			// { dg-error "" }
}
