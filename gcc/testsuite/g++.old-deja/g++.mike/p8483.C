// { dg-do run  }
// prms-id: 8483

int count;

class A {
public:
  A() { ++count; }
  ~A() { }
};

class B {
private:
  A b[2];  
};

class C {
public:
private:
  A c[2][2];
};

class D {
public:
private:
  A d[2][2][2];
};

int main() {
  { A a; }
  { B b; }
  { C c; }
  { D d; }
  if (count != 15)
    return 1;
}
