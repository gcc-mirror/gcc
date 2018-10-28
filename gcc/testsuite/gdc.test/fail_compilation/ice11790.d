/*
TEST_OUTPUT:
---
fail_compilation/ice11790.d(8): Error: cannot pass type string as a function argument
---
*/

string[string] crash = new string[string];
