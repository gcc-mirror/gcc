/*
TEST_OUTPUT:
---
fail_compilation/named_arguments_ifti_error.d(20): Error: template `f` is not callable using argument types `!()(int, int)`
fail_compilation/named_arguments_ifti_error.d(20):        parameter `x` assigned twice
fail_compilation/named_arguments_ifti_error.d(16):        Candidate is: `f(T, U)(T x, U y)`
fail_compilation/named_arguments_ifti_error.d(21): Error: template `f` is not callable using argument types `!()(int, int)`
fail_compilation/named_arguments_ifti_error.d(21):        argument `3` goes past end of parameter list
fail_compilation/named_arguments_ifti_error.d(16):        Candidate is: `f(T, U)(T x, U y)`
fail_compilation/named_arguments_ifti_error.d(22): Error: template `f` is not callable using argument types `!()(int)`
fail_compilation/named_arguments_ifti_error.d(22):        missing argument for parameter #1: `T x`
fail_compilation/named_arguments_ifti_error.d(16):        Candidate is: `f(T, U)(T x, U y)`
---
*/

void f(T, U)(T x, U y) {}

void main()
{
	f(x: 3, x: 3); // double assignment of x
	f(y: 3,    3); // overflow past last parameter
	f(y: 3);       // skipping parameter x
}
