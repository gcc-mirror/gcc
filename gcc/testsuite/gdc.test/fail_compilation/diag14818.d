/*
TEST_OUTPUT:
---
fail_compilation/diag14818.d(40): Error: none of the overloads of `func` are callable using argument types `(string)`
fail_compilation/diag14818.d(18):        Candidates are: `diag14818.foo(int _param_0)`
fail_compilation/diag14818.d(19):                        `diag14818.bar(double _param_0)`
fail_compilation/diag14818.d(41): Error: template instance `diag14818.X!string` does not match any template declaration
fail_compilation/diag14818.d(41):        Candidates are:
fail_compilation/diag14818.d(24):        Foo(T) if (is(T == int))
fail_compilation/diag14818.d(25):        Bar(T) if (is(T == double))
fail_compilation/diag14818.d(42): Error: template instance `diag14818.Y!string` does not match any template declaration
fail_compilation/diag14818.d(42):        Candidates are:
fail_compilation/diag14818.d(25):        Bar(T) if (is(T == double))
fail_compilation/diag14818.d(24):        Foo(T) if (is(T == int))
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
