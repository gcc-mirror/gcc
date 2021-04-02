/*
TEST_OUTPUT:
---
fail_compilation/fail322.d(13): Error: function `fail322.digestToString2(ref char[16] digest)` is not callable using argument types `(string)`
fail_compilation/fail322.d(13):        cannot pass rvalue argument `"1234567890123456"` of type `string` to parameter `ref char[16] digest`
fail_compilation/fail322.d(15): Error: function `fail322.digestToString2(ref char[16] digest)` is not callable using argument types `(const(char[16]))`
fail_compilation/fail322.d(15):        cannot pass argument `s` of type `const(char[16])` to parameter `ref char[16] digest`
---
*/

void main()
{
    digestToString2("1234567890123456");
    const char[16] s;
    digestToString2(s);
}

void digestToString2(ref char[16] digest)
{
    assert(digest[0] == 0xc3);
    assert(digest[15] == 0x3b);
}
