// PR c++/77626
// { dg-do compile }

struct B;			// { dg-message "forward declaration of" }
struct A { struct B b; };	// { dg-error "has incomplete type" }
void bar (int);

void
foo ()
{
  A a;
  bar ((int &) a);
}
