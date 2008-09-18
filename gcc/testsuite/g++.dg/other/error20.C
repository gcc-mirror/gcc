// PR c++/34275
// { dg-do compile }

struct A
{		// { dg-message "candidates" }
  virtual A foo ();
};

void bar (A& a)
{
  a.foo () = 0; // { dg-error "A::foo\\(\\) = 0" }
}   
