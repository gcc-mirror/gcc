/*
TEST_OUTPUT:
---
fail_compilation/ice8795b.d-mixin-8(8): Error: { } expected following `interface` declaration
fail_compilation/ice8795b.d-mixin-8(8): Error: anonymous interfaces not allowed
---
*/
mixin("interface;");
