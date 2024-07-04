/*
TEST_OUTPUT:
---
fail_compilation/named_arguments_ifti_error.d(17): Error: template `f` is not callable using argument types `!()(int, int)`
fail_compilation/named_arguments_ifti_error.d(13):        Candidate is: `f(T, U)(T x, U y)`
fail_compilation/named_arguments_ifti_error.d(18): Error: template `f` is not callable using argument types `!()(int, int)`
fail_compilation/named_arguments_ifti_error.d(13):        Candidate is: `f(T, U)(T x, U y)`
fail_compilation/named_arguments_ifti_error.d(19): Error: template `f` is not callable using argument types `!()(int)`
fail_compilation/named_arguments_ifti_error.d(13):        Candidate is: `f(T, U)(T x, U y)`
---
*/

void f(T, U)(T x, U y) {}

void main()
{
	f(x: 3, x: 3); // double assignment of x
	f(y: 3,    3); // overflow past last parameter
	f(y: 3);       // skipping parameter x
}
