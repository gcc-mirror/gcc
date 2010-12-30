// { dg-options "-fshow-column -fmessage-length=0   -ansi -pedantic-errors -Wno-long-long " }
// PR C++/17867

struct A			// { dg-message "8:operator=|no known conversion for implicit" }
{
  A(int);
};

const A& foo();

void bar()
{
  foo()=A(0); // { dg-error "12:no match for 'operator='" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 13 }
}
