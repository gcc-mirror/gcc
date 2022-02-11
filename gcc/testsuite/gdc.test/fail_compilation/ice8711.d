/*
TEST_OUTPUT:
---
fail_compilation/ice8711.d(8): Error: cannot use array to initialize `int function(int)`
---
*/

int function(int) foos = [x => 0];
