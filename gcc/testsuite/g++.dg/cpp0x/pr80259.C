// PR c++/80259
// { dg-do compile { target c++11 } }

void foo () {}	// { dg-message "previously defined here" }
void bar ();

struct A
{
  friend void foo () = delete;	// { dg-error "redefinition of" }
  friend void bar () = delete;	// { dg-message "previously defined here" }
};

void bar () {}	// { dg-error "redefinition of" }
