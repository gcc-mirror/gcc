/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-final { scan-hidden "_ZN1DaSERKS_" } } */

struct B {
  B& operator=(const B&);
};

struct __attribute__((visibility("hidden"))) D : public B {
  // The implicit assignment operator should be hidden.
};

__attribute__((visibility("hidden"))) D d1;
__attribute__((visibility("hidden"))) D d2;

void f() {
  d1 = d2;
}
