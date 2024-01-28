/*
TEST_OUTPUT:
----
fail_compilation/fail322.d(15): Error: function `digestToString2` is not callable using argument types `(string)`
fail_compilation/fail322.d(15):        cannot pass rvalue argument `"1234567890123456"` of type `string` to parameter `ref char[16] digest`
fail_compilation/fail322.d(20):        `fail322.digestToString2(ref char[16] digest)` declared here
fail_compilation/fail322.d(17): Error: function `digestToString2` is not callable using argument types `(const(char[16]))`
fail_compilation/fail322.d(17):        cannot pass argument `s` of type `const(char[16])` to parameter `ref char[16] digest`
fail_compilation/fail322.d(20):        `fail322.digestToString2(ref char[16] digest)` declared here
----
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
