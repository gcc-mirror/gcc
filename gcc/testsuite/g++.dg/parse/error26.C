// PR c++/20148
// { dg-options "-fshow-column -ansi -pedantic-errors -Wno-long-long" }

void foo()
{
  if (({int c[2];})) ; // { dg-error "7:ISO C.. forbids" "7" }
  // { dg-error "20:could not convert" "20" { target *-*-* } 6 }
}

void bar()
{
  if (({})); // { dg-error "7:ISO C.. forbids" "7" }
  // { dg-error "11:could not convert" "11" { target *-*-* } 12 }
}
