// PR c++/20148
// { dg-options "-fshow-column -ansi -pedantic-errors -Wno-long-long" }

void foo()
{
  if (({int c[2];})) ; // { dg-error "7:ISO C.. forbids" "7" }
  // { dg-error "7:could not convert" "17" { target *-*-* } .-1 }
}

void bar()
{
  if (({})); // { dg-error "7:ISO C.. forbids" "7" }
  // { dg-error "11:could not convert" "11" { target *-*-* } .-1 }
}
