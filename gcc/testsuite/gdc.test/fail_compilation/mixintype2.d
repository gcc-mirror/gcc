
/* TEST_OUTPUT:
---
fail_compilation/mixintype2.d(13): Error: alias `mixintype2.Foo.T` recursive alias declaration
fail_compilation/mixintype2.d(19): Error: `mixin(0)` does not give a valid type
fail_compilation/mixintype2.d(20): Error: unexpected token `{` after type `int()`
fail_compilation/mixintype2.d(20):        while parsing string mixin type `int() {}`
fail_compilation/mixintype2.d(20): Error: `mixin(_error_)` does not give a valid type
---
*/

struct Foo {
    alias T = mixin("T2");
}
alias T1 = mixin("Foo.T");
alias T2 = mixin("T1");
void func (T2 p) {}

enum mixin(0) a = 0;
mixin("int() {}") f;
