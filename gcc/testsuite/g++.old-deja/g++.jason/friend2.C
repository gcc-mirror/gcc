// { dg-do assemble  }
// PRMS Id: 5189
// Bug: g++ fails to collapse the several declarations of freefoo, so it isn't
// recognized as a friend.

extern "C"
void freefoo(void);

class foo {
   friend void freefoo(void);
   protected:
      static void foomem();
   public:
      foo();
      ~foo();
};

void freefoo(void)
{
   foo::foomem();
}
