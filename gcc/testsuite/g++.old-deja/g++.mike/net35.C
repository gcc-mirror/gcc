extern "C" int printf(const char *, ...);

class A {
 public:
   char *x;
};

class B1:public virtual A { };

class B2:public virtual A { };

class C:public B1, public B2 {
public:
  C() { }
};

int main() {
  C c;
  printf("&c.x = %x\n", &c.x);
  printf("&c.B1::x = %x\n", &c.B1::x);
  printf("&c.B2::x = %x\n", &c.B2::x);
  printf("&c.A::x = %x\n", &c.A::x);
  if (&c.x != &c.B1::x
      || &c.x != &c.B2::x
      || &c.x != &c.A::x)
    return 1;
}
