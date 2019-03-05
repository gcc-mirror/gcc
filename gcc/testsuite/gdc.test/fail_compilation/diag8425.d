/*
REQUIRED_ARGS: -m64 -o-
PERMUTE_ARGS:
TEST_OUTPUT:
---
fail_compilation/diag8425.d(13): Error: T in __vector(T) must be a static array, not void
fail_compilation/diag8425.d(14): Error: 1 byte vector type __vector(void[1]) is not supported on this platform
fail_compilation/diag8425.d(15): Error: 99 byte vector type __vector(void[99]) is not supported on this platform
fail_compilation/diag8425.d(16): Error: vector type __vector(void*[4]) is not supported on this platform
---
*/

alias a = __vector(void); // not static array
alias b = __vector(void[1]); // wrong size
alias c = __vector(void[99]); // wrong size
alias d = __vector(void*[4]); // wrong base type
