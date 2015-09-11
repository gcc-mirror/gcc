// PR sanitizer/65000
// { dg-do compile }
// { dg-options "-O1 -fsanitize=undefined -fno-sanitize-recover" }

struct B { virtual ~B () {} void foo (); };
struct C { virtual ~C (); };
struct A : public virtual C {};
struct D : A { ~D () { d.foo (); } B d; };

void
bar ()
{
  D a;
}
