// { dg-do compile }
// Testcase by: bangerth@dealii.org
// PR c++/10858: failure with calling a method inside sizeof in a template

    template <int> struct P {};
    
    void bar ();
    
    template <class T> struct X {
        static int foo(void (*)());
        P<sizeof(foo(&bar))> p;    
    };
    
    template class X<int>;
