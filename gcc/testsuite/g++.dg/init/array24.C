// PR c++/29175
// { dg-options "" }

void foo(int i)
{
  int x[][i] = { 0 }; // { dg-error "variable-sized|storage size" }
}
