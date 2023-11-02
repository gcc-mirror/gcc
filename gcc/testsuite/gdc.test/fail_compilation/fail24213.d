/+
REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/fail24213.d(16): Error: reference to local variable `n` assigned to non-scope parameter `p`
---
+/
alias Dg = void delegate(int* p) @safe pure nothrow;

void main() @safe
{
    int* escaped;

    int n;
    Dg dg = delegate void (int* p) { escaped = p; };
    dg(&n);
}
