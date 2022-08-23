/*
REQUIRED_ARGS: -verrors=spec -o-
TEST_OUTPUT:
---
(spec:1) fail_compilation/fail20730b.d-mixin-43(43): Error: C style cast illegal, use `cast(int)mod`
fail_compilation/fail20730b.d(26): Error: none of the overloads of template `fail20730b.atomicOp` are callable using argument types `!("+=")(shared(uint), int)`
fail_compilation/fail20730b.d(41):        Candidate is: `atomicOp(string op, T, V1)(shared ref T val, V1 mod)`
  with `op = "+=",
       T = uint,
       V1 = int`
  must satisfy the following constraint:
`       __traits(compiles, mixin("(int)mod"))`
---
*/
void test20730()
{
    auto f = File().byLine;
}

struct File
{
    shared uint refs;

    this(this)
    {
        atomicOp!"+="(refs, 1);
    }

    struct ByLineImpl(Char)
    {
        File file;
        char[] line;
    }

    auto byLine()
    {
        return ByLineImpl!char();
    }
}

T atomicOp(string op, T, V1)(ref shared T val, V1 mod)
    // C-style cast causes raises a parser error whilst gagged.
    if (__traits(compiles, mixin("(int)mod")))
{
    return val;
}
