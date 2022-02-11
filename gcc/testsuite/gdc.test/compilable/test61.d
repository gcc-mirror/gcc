// PERMUTE_ARGS:
// EXTRA_FILES: imports/test61a.d
import imports.test61a;
alias imports.test61a.bar bar;

mixin(`
enum FooB { fooB };
void bar(FooB x) {}
`);

void test()
{
    bar(FooA.fooA);
    bar(FooB.fooB);
}
