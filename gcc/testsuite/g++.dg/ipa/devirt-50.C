/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized"  } */
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

/* { dg-final { scan-tree-dump "cxa_pure_virtual" "optimized"} } */
