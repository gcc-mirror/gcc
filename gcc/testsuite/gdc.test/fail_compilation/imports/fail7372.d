module imports.fail7372;
import imports.imp1;
mixin template Issue7372()
{
    public void f()
    {
        int foo = X;
    }
}
