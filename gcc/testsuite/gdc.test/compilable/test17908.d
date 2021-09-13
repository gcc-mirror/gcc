// PERMUTE ARGS:

@disable void foo() {}
void foo(int) {}
alias g = foo;

// make sure the order of declaration
// doesn't change anything
void bar(int) {}
@disable void bar() {}
alias h = bar;

void main()
{
    g(10);
    h(10);
}
