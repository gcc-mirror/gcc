// PR c++/25417
// { dg-options "" }

struct object {
  int a;
  int b;
};

void f (int c, int d)
{
  object o = ((object){ a : c, b : d});
}
