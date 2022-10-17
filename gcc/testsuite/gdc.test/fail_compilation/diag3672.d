// REQUIRED_ARGS: -de
/*
TEST_OUTPUT:
---
fail_compilation/diag3672.d(8): Error: read-modify-write operations are not allowed for `shared` variables
fail_compilation/diag3672.d(8):        Use `core.atomic.atomicOp!"+="(x, 1)` instead
fail_compilation/diag3672.d(9): Error: read-modify-write operations are not allowed for `shared` variables
fail_compilation/diag3672.d(9):        Use `core.atomic.atomicOp!"+="(x, 1)` instead
fail_compilation/diag3672.d(10): Error: read-modify-write operations are not allowed for `shared` variables
fail_compilation/diag3672.d(10):        Use `core.atomic.atomicOp!"-="(x, 1)` instead
fail_compilation/diag3672.d(11): Error: read-modify-write operations are not allowed for `shared` variables
fail_compilation/diag3672.d(11):        Use `core.atomic.atomicOp!"-="(x, 1)` instead
fail_compilation/diag3672.d(12): Error: read-modify-write operations are not allowed for `shared` variables
fail_compilation/diag3672.d(12):        Use `core.atomic.atomicOp!"+="(x, 1)` instead
fail_compilation/diag3672.d(13): Error: read-modify-write operations are not allowed for `shared` variables
fail_compilation/diag3672.d(13):        Use `core.atomic.atomicOp!"+="(x, 2)` instead
fail_compilation/diag3672.d(14): Error: read-modify-write operations are not allowed for `shared` variables
fail_compilation/diag3672.d(14):        Use `core.atomic.atomicOp!"-="(x, 3)` instead
fail_compilation/diag3672.d(15): Error: read-modify-write operations are not allowed for `shared` variables
fail_compilation/diag3672.d(15):        Use `core.atomic.atomicOp!"|="(x, y)` instead
fail_compilation/diag3672.d(16): Error: read-modify-write operations are not allowed for `shared` variables
fail_compilation/diag3672.d(16):        Use `core.atomic.atomicOp!"*="(x, y)` instead
fail_compilation/diag3672.d(17): Error: read-modify-write operations are not allowed for `shared` variables
fail_compilation/diag3672.d(17):        Use `core.atomic.atomicOp!"/="(x, y)` instead
fail_compilation/diag3672.d(18): Error: read-modify-write operations are not allowed for `shared` variables
fail_compilation/diag3672.d(18):        Use `core.atomic.atomicOp!"%="(x, y)` instead
fail_compilation/diag3672.d(19): Error: read-modify-write operations are not allowed for `shared` variables
fail_compilation/diag3672.d(19):        Use `core.atomic.atomicOp!"&="(x, y)` instead
fail_compilation/diag3672.d(20): Error: read-modify-write operations are not allowed for `shared` variables
fail_compilation/diag3672.d(20):        Use `core.atomic.atomicOp!"^="(x, y)` instead
fail_compilation/diag3672.d(21): Error: read-modify-write operations are not allowed for `shared` variables
fail_compilation/diag3672.d(21):        Use `core.atomic.atomicOp!"<<="(x, y)` instead
fail_compilation/diag3672.d(22): Error: read-modify-write operations are not allowed for `shared` variables
fail_compilation/diag3672.d(22):        Use `core.atomic.atomicOp!">>="(x, y)` instead
fail_compilation/diag3672.d(23): Error: read-modify-write operations are not allowed for `shared` variables
fail_compilation/diag3672.d(23):        Use `core.atomic.atomicOp!">>>="(x, y)` instead
fail_compilation/diag3672.d(24): Error: read-modify-write operations are not allowed for `shared` variables
fail_compilation/diag3672.d(24):        Use `core.atomic.atomicOp!"^^="(x, y)` instead
fail_compilation/diag3672.d(25): Error: read-modify-write operations are not allowed for `shared` variables
fail_compilation/diag3672.d(25):        Use `core.atomic.atomicOp!"+="(ptr, 1)` instead
fail_compilation/diag3672.d(26): Error: read-modify-write operations are not allowed for `shared` variables
fail_compilation/diag3672.d(26):        Use `core.atomic.atomicOp!"+="(ptr, 1)` instead
fail_compilation/diag3672.d(27): Error: read-modify-write operations are not allowed for `shared` variables
fail_compilation/diag3672.d(27):        Use `core.atomic.atomicOp!"-="(ptr, 1)` instead
fail_compilation/diag3672.d(28): Error: read-modify-write operations are not allowed for `shared` variables
fail_compilation/diag3672.d(28):        Use `core.atomic.atomicOp!"-="(ptr, 1)` instead
---
*/

#line 1
shared int x;
shared int y;
shared int* ptr;
shared static this() { ptr = new int; } // silence null-dereference errors

void main()
{
    ++x;
    x++;
    --x;
    x--;
    x += 1;
    x += 2;
    x -= 3;
    x |= y;
    x *= y;
    x /= y;
    x %= y;
    x &= y;
    x ^= y;
    x <<= y;
    x >>= y;
    x >>>= y;
    x ^^= y;
    ++ptr;
    ptr++;
    --ptr;
    ptr--;
}
