// { dg-do run  }
// This testcase ensures that we can build vtable names for complex MI
// classes.

class C_A {
public:
  virtual int foo(void *) { return  0; }
} a;
 
class C_B : public C_A {
} b;
 
class C_C : public C_A {
} c;
 
class C_D : public C_A {
} d;
 
class C_E : public C_C, public C_B {
public:
  virtual int foo(void *) { return 0; }
} e;
 
class C_F : public C_D, public C_B {
public:
  virtual int foo(void *) { return 0; }
} f;
 
class C_G : public C_A {
public:
  virtual int foo(void *) { return 0; }
} g;
 
class C_H : public C_G, public C_E, public C_F {
public:
  virtual int foo(void *) { return 0; }
} h;
 
int main() {
}
