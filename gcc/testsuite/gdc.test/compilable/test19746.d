// REQUIRED_ARGS: -Icompilable/imports
// EXTRA_FILES: imports/test19746a.d imports/test19746b.d imports/test19746c.d imports/test19746d.d
import test19746c;
import test19746b: Frop;

template Base(T)
{
    static if (is(T == super)) alias Base = Object;
}

class Foo
{
    class Nested: Base!Foo { }
    void func(Frop) { }
    void thunk() { }
}
