// PR sanitizer/95693
// { dg-do run }
// { dg-options "-O2 -fsanitize=undefined -fno-sanitize-recover=undefined" }

int g = 9;

struct A {
  A () : a(g) {}
private:
  int &a;
};

struct B {
  A payload;
};

struct C : public B {
  C () : B () {}
  A p;
};

int
main ()
{
  C t;
}
