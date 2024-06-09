/*
TEST_OUTPUT:
---
fail_compilation/iconv_interface_array.d(48): Error: function `testA` is not callable using argument types `(C[4])`
fail_compilation/iconv_interface_array.d(48):        cannot pass argument `arr` of type `C[4]` to parameter `I1[4] arr`
fail_compilation/iconv_interface_array.d(27):        `iconv_interface_array.testA(I1[4] arr)` declared here
fail_compilation/iconv_interface_array.d(49): Error: function `testB` is not callable using argument types `(C[4])`
fail_compilation/iconv_interface_array.d(49):        cannot pass argument `arr` of type `C[4]` to parameter `I2[4] arr`
fail_compilation/iconv_interface_array.d(33):        `iconv_interface_array.testB(I2[4] arr)` declared here
fail_compilation/iconv_interface_array.d(50): Error: function `testC` is not callable using argument types `(C[4])`
fail_compilation/iconv_interface_array.d(50):        cannot pass argument `arr` of type `C[4]` to parameter `I3[4] arr`
fail_compilation/iconv_interface_array.d(39):        `iconv_interface_array.testC(I3[4] arr)` declared here
---
*/

interface I1 { int a(int); }
interface I2 { int b(int); }
interface I3 { int c(int); }

class C : I1, I2, I3
{
    int a(int i) { return 1 * i; }
    int b(int i) { return 2 * i; }
    int c(int i) { return 3 * i; }
}

void testA(I1[4] arr)
{
    foreach (uint idx, obj; arr)
        assert(obj.a(idx) == 1 * idx);
}

void testB(I2[4] arr)
{
    foreach (idx, obj; arr)
        assert(obj.b(cast(int) idx) == 2 * idx);
}

void testC(I3[4] arr)
{
    foreach (idx, obj; arr)
        assert(obj.c(cast(int) idx) == 3 * idx);
}

void main()
{
    C[4] arr = [ new C, new C, new C, new C ];
    testA(arr);
    testB(arr);
    testC(arr);
}
