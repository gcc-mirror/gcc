// PR c++/59097
// { dg-options "" }

void foo()
{
  int x[({ return; })];		// { dg-error "could not convert" }
// { dg-error "9:size of array .x. has non-integral" "" { target *-*-* } .-1 }  
}
