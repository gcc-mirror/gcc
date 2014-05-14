// { dg-options "-fshow-column -fmessage-length=0   -ansi -pedantic-errors -Wno-long-long " }
// PR C++/17867

struct A			// { dg-message "8:operator=" }
{
  A(int);
};

const A& foo();

void bar()
{
  foo()=A(0); // { dg-error "8:no match|const" }
}
