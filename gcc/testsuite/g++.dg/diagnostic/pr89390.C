// PR c++/89390
// { dg-do compile { target c++11 } }

enum class A { B, C };

void
foo ()
{
  A::~A ();	// { dg-error "'~A' is not a member of 'A'" "" { target *-*-* } 0 }
}
