// { dg-lto-do link }
// { dg-lto-options { "-O2 -flto" }  } 
struct B {
  enum class E { V0, V1 };
  virtual ~B();
  E e;
};

B b;

int main() {}
