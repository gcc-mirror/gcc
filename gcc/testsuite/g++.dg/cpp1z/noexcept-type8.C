// Test for exception handling.
// { dg-options -std=c++17 }
// { dg-do run }

void f() {}
void g() noexcept {}

int main()
{
  try { throw g; }
  catch (void (*)()) { }

  try { throw g; }
  catch (void (*)() noexcept) { }

  try { throw f; }
  catch (void (*)()) { }

  try { throw f; }
  catch (void (*)() noexcept) { __builtin_abort(); }
  catch (...) { }
}
