// { dg-do assemble  }
// { dg-options "-O2" }
// Origin: Matt Austern <austern@isolde.engr.sgi.com>

class X;

extern X* tab1;

struct Y {
  explicit Y(int);
};

void* x ();

Y k (void *);

inline void f() { k (x ()); }

inline void* x () 
{
  return 0;
}

static void g() {
  f();
}

static void h() {
  f();
}
