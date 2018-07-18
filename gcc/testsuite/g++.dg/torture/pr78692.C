// PR c++/78692

int a;
void *b;
extern "C" {
struct C {
  virtual int d ();
};
struct E {
  virtual int operator () (int, const void *, int) = 0;
};
class F {
  int g ();
  int h;
  E &i;
};
struct : C, E {
  int operator () (int, const void *, int) { throw int(); }
} j;

int
F::g ()
{
  a = i (h, b, 0);
  return 0;
}
}
