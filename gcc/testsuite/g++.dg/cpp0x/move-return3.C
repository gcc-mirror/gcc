// PR c++/91212
// Test that C++11 implicit move semantics don't call the const copy.
// { dg-do link }

struct T { int i; };

struct X {
  X(T&) { }    // #1
  X(const T&); // #2
};

X
fn ()
{
  T buf;
  return buf;
}

int
main()
{
  X c = fn ();
}
