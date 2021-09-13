
/* TEST_OUTPUT:
---
fail_compilation/mixintype2.d(10): Error: alias `mixintype2.Foo.T` recursive alias declaration
fail_compilation/mixintype2.d(16): Error: `mixin(0)` does not give a valid type
---
*/

struct Foo {
    alias T = mixin("T2");
}
alias T1 = mixin("Foo.T");
alias T2 = mixin("T1");
void func (T2 p) {}

enum mixin(0) a = 0;
