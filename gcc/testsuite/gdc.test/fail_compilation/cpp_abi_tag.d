/* DISABLED: win32 win64
REQUIRED_ARGS: -extern-std=c++11
TEST_OUTPUT:
---
fail_compilation/cpp_abi_tag.d(111): Error: `@gnuAbiTag` can only apply to C++ symbols
fail_compilation/cpp_abi_tag.d(131): Error: `@gnuAbiTag` cannot be applied to namespaces
fail_compilation/cpp_abi_tag.d(102): Error: `@gnuAbiTag` at least one argument expected
fail_compilation/cpp_abi_tag.d(105): Error: `@gnuAbiTag` at least one argument expected
fail_compilation/cpp_abi_tag.d(108): Error: `@gnuAbiTag` char `0x99` not allowed in mangling
fail_compilation/cpp_abi_tag.d(114): Error: argument `2` to `@gnuAbiTag` cannot be `null`
fail_compilation/cpp_abi_tag.d(114): Error: argument `3` to `@gnuAbiTag` cannot be empty
fail_compilation/cpp_abi_tag.d(117): Error: `@gnuAbiTag` at least one argument expected
fail_compilation/cpp_abi_tag.d(137): Error: only one `@gnuAbiTag` allowed per symbol
fail_compilation/cpp_abi_tag.d(137):        instead of `@gnuAbiTag(["x"]) @gnuAbiTag(["a"])`, use `@gnuAbiTag("x", "a")`
---
*/

#line 100
import core.attribute;

@gnuAbiTag
extern(C++) struct A {}

@gnuAbiTag()
extern(C++) struct B {}

@gnuAbiTag("a\x99")
extern(C++) struct D {}

@gnuAbiTag("a")
struct F {}

@gnuAbiTag("a", null, "")
extern(C++) struct G {}

@gnuAbiTag((string[]).init)
extern(C++) struct H {}

// Note: There is no way to distinguish between
// `extern(C++, "ns") { ... }` and `extern(C++, "ns") ...;`
// So ABI tags have to be on the inside
extern(C++, "ns") @gnuAbiTag("x") void func1();
extern(C++, ns2)  @gnuAbiTag("x") void  func2();

@gnuAbiTag("x")
extern(C++, "ns3")
{
    void func3();
}
@gnuAbiTag("x")
extern(C++, ns4)
{
    void func4();
}

@gnuAbiTag("x") @gnuAbiTag("a")
extern(C++) void func5();
