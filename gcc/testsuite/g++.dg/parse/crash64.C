// PR c++/58565
// { dg-options "" }

void foo()
{
  int i = ({ L: ; });  // { dg-error "void value not ignored" }
}
