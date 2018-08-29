// { dg-do compile }
// { dg-options "-O1 -fno-ipa-pure-const" }
struct B {
        B* self;
        B() : self( this ) { self->f(); }
        virtual void f() = 0;
    };

    struct D : B
    {
        void f() {}
    };

    int main()
    {
        D d;
    }
