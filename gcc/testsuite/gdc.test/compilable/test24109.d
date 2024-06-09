// https://issues.dlang.org/show_bug.cgi?id=24109

struct Outer
{
    void method1() {}

    void method2()
    {
        class Inner
        {
            void innerMethod()
            {
                method1();
            }
        }
    }
}
