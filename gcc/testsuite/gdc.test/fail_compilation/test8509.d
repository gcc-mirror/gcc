/*
TEST_OUTPUT:
---
fail_compilation/test8509.d(13): Error: cannot implicitly convert expression `"hello world"` of type `string` to `E`
fail_compilation/test8509.d(14): Error: cannot implicitly convert expression `"hello world"` of type `string` to `E`
---
*/
module test8509;
enum E : string { a = "hello", b = "world" }

void main()
{
    E e1 = E.a ~ " world";
    E e2 = "hello " ~ E.b;
}
