// { dg-do link  }
// { dg-options "-fno-implement-inlines " }
struct type {
  virtual void m1();
  virtual void m2() { }
};

void type::m1() { }

int main() {
  type t;
}
