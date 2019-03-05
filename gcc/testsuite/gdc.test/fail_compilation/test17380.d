/* TEST_OUTPUT:
---
fail_compilation/test17380.d(12): Error: undefined identifier `ThisTypeDoesNotExistsAndCrahesTheCompiler`
---
 * https://issues.dlang.org/show_bug.cgi?id=17380
 */

struct Int128
{
    Uint128 opCast()
    {
        return ThisTypeDoesNotExistsAndCrahesTheCompiler;
    }
    alias opCast this;
}

struct Uint128
{
    Int128 opCast() { return Int128.init; }
    alias opCast this;
}
