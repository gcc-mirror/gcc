
class A {}
interface B {}
interface C {}
interface D(X) {}

void fun()
{
    class T : typeof(new A), .B, const(C), D!int {}
    version(none)
    {
        class U : int, float, __vector(int[3]) {}
    }
}
