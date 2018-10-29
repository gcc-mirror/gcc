/*
TEST_OUTPUT:
---
fail_compilation/diag14818.d(34): Error: none of the overloads of 'func' are callable using argument types (string), candidates are:
fail_compilation/diag14818.d(12):        diag14818.foo(int _param_0)
fail_compilation/diag14818.d(13):        diag14818.bar(double _param_0)
fail_compilation/diag14818.d(35): Error: overload alias diag14818.X does not match any template declaration
fail_compilation/diag14818.d(36): Error: overloadset diag14818.M does not match any template declaration
---
*/

void foo(int) {}
void bar(double) {}
alias func = foo;
alias func = bar;
// in here, func is a FuncAliasDeclaration;

template Foo(T) if (is(T == int)) {}
template Bar(T) if (is(T == double)) {}

alias X = Foo;
alias X = Bar;
// in here, X is an OverDeclaration

template Mix1() { alias M = Foo; }
template Mix2() { alias M = Bar; }
mixin Mix1;
mixin Mix2;
alias Y = M;
// in here, Y is an OverloadSet

void main()
{
    func("abc");
    alias x = X!string;
    alias y = Y!string;
}
