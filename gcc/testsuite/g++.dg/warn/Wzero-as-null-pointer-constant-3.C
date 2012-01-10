// PR c++/51290
// { dg-options "-Wzero-as-null-pointer-constant" }

class A { int a; };

class B { int b; };

class C : public A, public B
{
    private:
        static void foo (A *x)
        {
            C *y = static_cast<C *>(x);
            (void) y;
        }

        static void bar (B *x)
        {
            C *y = static_cast<C *>(x);
            (void) y;
        }
};
