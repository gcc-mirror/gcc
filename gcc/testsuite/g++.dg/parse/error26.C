// PR c++/20148
// { dg-options "-fshow-column -ansi -pedantic-errors -Wno-long-long" }

void foo()
{
  if (({int c[2];})) ; // { dg-error "7: error: ISO C.. forbids|20: error: could not convert" }
}

void bar()
{
  if (({})); // { dg-error "7: error: ISO C.. forbids|11: error: could not convert" }
}
