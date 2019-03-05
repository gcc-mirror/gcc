// REQUIRED_ARGS: -o-
/*
TEST_OUTPUT:
---
fail_compilation/diag13787.d(12): Error: cannot slice function pointer & main
fail_compilation/diag13787.d(13): Error: cannot index function pointer & main
---
*/

void main()
{
    auto a = (&main)[0..1];
    auto x = (&main)[0];
}
