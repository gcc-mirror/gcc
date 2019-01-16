module protection.aggregate.mod14275;

public struct Foo
{
    package(protection) void foo() {}
    package void foo2() {}
}

package(protection) void bar()
{
}
