module imports.test15785;

interface IBase2
{
    final protected void faz() {}
}

class Base
{
    protected void foo() {}
    protected void bar() {}
    protected alias T = int;
}
