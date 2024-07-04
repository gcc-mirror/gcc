/**
REQUIRED_ARGS: -m64
TEST_OUTPUT:
---
fail_compilation/vector_cast.d(11): Error: cannot cast expression `a` of type `int[3]` to `__vector(int[4])`
fail_compilation/vector_cast.d(13): Error: cannot cast expression `a` of type `int[5]` to `__vector(int[4])`
---
*/

alias int4 = __vector(int[4]);
int4 convtest3(int[3] a) { return cast(int4) a; }
int4 convtest4(int[4] a) { return cast(int4) a; }
int4 convtest5(int[5] a) { return cast(int4) a; }
