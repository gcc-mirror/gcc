// PR ipa/66896
// { dg-do compile }

void f2 (void *);
void f3 ();

struct A
{
  int *a;
  A ();
  ~A () { a3 (); }
  void a1 (int * p) { if (!p) f3 (); f2 (p); }
  void a3 () { if (*a) a1 (a); }
};

struct B : A {~B () { a3 ();}};

struct F {};

struct G : F {B g;};

void foo () {G g;}
