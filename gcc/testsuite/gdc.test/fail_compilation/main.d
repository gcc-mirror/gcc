/*
TEST_OUTPUT:
---
fail_compilation/main.d(9): Error: only one entry point `main`$?:windows=, `WinMain` or `DllMain`$ is allowed
fail_compilation/main.d(8):        previously found `void main()` here
---
*/
void main() {}
void main(string[] args) {}
