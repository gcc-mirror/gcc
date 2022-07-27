// REQUIRED_ARGS: -de
/*
TEST_OUTPUT:
---
fail_compilation/fail16001.d(10): Deprecation: using `(args) => { ... }` to create a delegate that returns a delegate is error-prone.
fail_compilation/fail16001.d(10):        Use `(args) { ... }` for a multi-statement function literal or use `(args) => () { }` if you intended for the lambda to return a delegate.
---
*/
void main() {
	auto fail = () => {};
	auto ok = () => () {};
}
