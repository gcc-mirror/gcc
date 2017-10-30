// { dg-do run { target c++11 } }
// PR82560, failed to destruct default arg inside new

static int liveness = 0;

struct Foo {

  Foo (int) {
    liveness++;
  }

  ~Foo() {
    liveness--;
  }

};

struct Bar {
  Bar (Foo = 0) { }
  ~Bar() { }
};

int main()
{
  delete new Bar();

  return liveness != 0;;
}
