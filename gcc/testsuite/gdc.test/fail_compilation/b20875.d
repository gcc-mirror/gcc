module b20875;

/*
TEST_OUTPUT:
---
fail_compilation/b20875.d(10): Error: template instance `Foo!int` does not match template declaration `Foo(alias T : None!U, U...)`
fail_compilation/b20875.d(10):        while evaluating: `static assert(Foo!int)`
fail_compilation/b20875.d(11): Error: template instance `Bar!int` does not match template declaration `Bar(alias T : None!U, U...)`
fail_compilation/b20875.d(11):        while evaluating: `static assert(!Bar!int)`
fail_compilation/b20875.d(14): Error: template parameter specialization for a type must be a type and not `NotAType()`
fail_compilation/b20875.d(15):        while looking for match for `Baz!int`
fail_compilation/b20875.d(15):        while evaluating: `static assert(!Baz!int)`
---
*/

#line 7

enum Foo(alias T : None!U, U...) = true;
enum Bar(alias T : None!U, U...) = false;
static assert( Foo!(int));
static assert(!Bar!(int));

template NotAType(){}
enum Baz(alias T : NotAType) = false;
static assert(!Baz!(int));

void main(){}
