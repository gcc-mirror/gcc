/**
TEST_OUTPUT:
---
fail_compilation/named_arguments_parse.d(10): Error: named arguments not allowed here
fail_compilation/named_arguments_parse.d(13): Error: named arguments not allowed here
fail_compilation/named_arguments_parse.d(14): Error: named arguments not allowed here
---
*/

@(attribute: 3)
void main()
{
	mixin(thecode: "{}");
	pragma(msg, themsg: "hello");
}
