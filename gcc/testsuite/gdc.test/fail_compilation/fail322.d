/*
TEST_OUTPUT:
---
fail_compilation/fail322.d(10): Error: function fail322.digestToString2 (ref char[16] digest) is not callable using argument types (string)
---
*/

void main()
{
    digestToString2("1234567890123456");
}

void digestToString2(ref char[16] digest)
{
    assert(digest[0] == 0xc3);
    assert(digest[15] == 0x3b);
}
