// PR c++/84664
// { dg-do compile { target c++11 } }

void
foo ()
{
  auto &b = 1; // { dg-error "cannot bind" }
  [] { b > 0; }; // { dg-error ".b. is not captured" }
}
