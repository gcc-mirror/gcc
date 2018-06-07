// PR 84812.  ICE determining implicit "C" linkage

struct A { void foo(); };
struct B { void foo(); };

struct C : A, B
{
  void X ();
};

void C::X ()
{
  void foo (); // local decl of ::foo

  foo ();
}

// { dg-final { scan-assembler "_Z3foov" } }
