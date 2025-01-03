// PR c++/118275
// { dg-do compile }
// { dg-options "" }

struct A { int a; char b[]; };
void bar (A *);

void
foo ()
{
  static struct A a = { .a = 1, .b = {
#embed __FILE__
				     } };
  bar (&a);
}
