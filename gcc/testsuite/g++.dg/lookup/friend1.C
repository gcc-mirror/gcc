// { dg-do compile }

// Origin: <matz@suse.de>

// PR c++/9970: In-class friend function definition after
// declaration lexical scope problem.

void f();
struct X
{
  enum { k = 1 };
  friend void f() {
        char a[k];
  }
};
