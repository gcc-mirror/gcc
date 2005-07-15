// { dg-do run }
// PR c++/22132

extern "C" void abort ();

struct foo {
  int a;
  int b;
};

class Foobar : public foo {
public:
  Foobar() { a = 1; b = 2; };
  virtual ~Foobar() {};
};

Foobar obj;
const Foobar* objPtr = &obj;
foo* f = (foo*)objPtr;

int main () {
  if (f->a != 1 || f->b != 2)
    abort ();
}

