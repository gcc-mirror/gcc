// Bug: g++ doesn't keep track of the lexical context of friends properly.

extern "C" void exit(int);

struct B;
struct A {
  static void f () { exit (1); }
};

struct B {
  static void f () { exit (0); }
  friend void g () { f (); }
};

int main ()
{
  g ();
}
