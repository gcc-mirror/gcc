interface I_Foo { void i_outer(); }
class C_Foo { void c_outer() { } }

class Bar
{
    interface I_Foo { void i_inner(); }
    class C_Foo { void c_inner() { } }
    
    class Impl1 : C_Foo, I_Foo
    {
        override void i_inner() { }
        override void c_inner() { }
    }
    
    class Impl2 : C_Foo, .I_Foo
    {
        override void i_outer() { }
        override void c_inner() { }
    }
    
    class Impl3 : .C_Foo, I_Foo
    {
        override void i_inner() { }
        override void c_outer() { }
    }
    
    class Impl4 : .C_Foo, .I_Foo
    {
        override void i_outer() { }
        override void c_outer() { }
    }
}
