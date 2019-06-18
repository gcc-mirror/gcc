import test19655f;
import test19655g;
class Grault: Bar
{
    void func()
    {
      func2;
    }
    void func1()
    {
        assert(false, "func1 was never called");
    }
    void func2() { }
}
