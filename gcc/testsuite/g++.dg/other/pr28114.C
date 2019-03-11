
// Test to make sure we do not ICE on this invalid program.

template<int> void foo(struct {}*){} // { dg-error "may not be defined" }

void bar()
{
  foo<0>(0);
}
