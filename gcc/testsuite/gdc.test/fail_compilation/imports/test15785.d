module imports.test15785;

class Base
{
    private void foo() {}
    private void bar() {}
    private alias T = int;
}

interface IBase2
{
    private alias T = int;
}
