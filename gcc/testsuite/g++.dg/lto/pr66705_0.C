// { dg-lto-do link }
// { dg-lto-options { { -O2 -flto -flto-partition=max -fipa-pta } } }
// { dg-extra-ld-options "-r -nostdlib" }

class A {
public:
    A();
};
int a = 0;
void foo() {
    a = 0;
    A b;
    for (; a;)
      ;
}
