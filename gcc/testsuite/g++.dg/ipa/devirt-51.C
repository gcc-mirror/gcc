/* Be sure we do not optimize the virtual call into call of the only non-virtual
   variant.  Either keeping virtual call or optimizing to cxa_pure_virtual
   is fine.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fsanitize=unreachable -fdump-tree-optimized"  } */
namespace {
  struct B {
        B* self;
        B() : self( this ) { self->f(); }
	void E(void);
        virtual void f() = 0;
    };

    struct D : B
    {
        void f() {}
    };
}

struct D e;

__attribute__ ((used))
void B::E(void)
  {
    this->f();
}

    int main()
    {
        D d;
    }
/* { dg-final { scan-tree-dump "cxa_pure_virtual" "optimized"  } } */
