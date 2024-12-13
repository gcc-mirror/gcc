// Check that contract assertion on a non-empty statement doesn't cause an
// ICE.
// { dg-options "-std=c++2a -fcontracts " }

void f();
int main ()
{
  [[assert: true]] f(); // { dg-error "assertions must be followed by" }
}
