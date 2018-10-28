/*
TEST_OUTPUT:
---
fail_compilation/ice11944.d(12): Error: template instance doCommand!(func) does not match template declaration doCommand(f, T)(f, T arg)
---
*/

void func(int var) {}

void doCommand(f, T)(f, T arg) {}

auto var = &doCommand!func;
