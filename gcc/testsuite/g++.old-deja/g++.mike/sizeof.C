// { dg-do compile }

class Foo {
  int children[4];
public:
  unsigned function(void) {
    return sizeof (((Foo*)0)->children);
  }
};
