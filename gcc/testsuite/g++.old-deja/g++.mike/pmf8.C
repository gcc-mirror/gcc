int fail;

class A *ptr_a;

class A {
public:
  char space1[24];
  virtual void foo() {
    if (this != ptr_a)
      fail = 1;
  }
};

class Space {
  char space2[36];
};

class B : public Space, public A {
} b;

void (B::*pmf1)() = &A::foo;
void (A::*pmf2)() = &A::foo;

int main() {
  ptr_a = &b;
  (b .* (void (B::*) ()) &A::foo) ();
  (b .* pmf1) ();
  (b .* pmf2) ();
  (b .* &A::foo) ();
  return fail;
}
