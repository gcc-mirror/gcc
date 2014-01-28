// PR c++/59097
// { dg-options "" }

void foo()
{
  int x[({ return; })];		// { dg-error "non-integral" }
}
