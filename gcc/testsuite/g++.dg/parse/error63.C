// PR c++/96137
// { dg-do compile }

void
fn ()
{
  X.operator T(); // { dg-error ".X. was not declared in this scope|expected" }
}
