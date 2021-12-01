/* 
TEST_OUTPUT:
---
fail_compilation/ccast.d(9): Error: C style cast illegal, use `cast(byte)i`
---
*/

int i;
byte b = (byte)i;
