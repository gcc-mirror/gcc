void foo() {}
int _foo;
alias _foo foo;

void main()
{
    foo = 1;
}

