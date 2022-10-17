/*
TEST_OUTPUT:
---
fail_compilation/b19730.d(10): Error: found `)` while expecting `=` or identifier
fail_compilation/b19730.d(11): Error: found `)` while expecting `=` or identifier
---
*/
void func() {
  bool x;
  if (const x) {}
  if (auto x) {}
}
