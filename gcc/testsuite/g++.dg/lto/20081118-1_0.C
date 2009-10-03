/* { dg-lto-do link } */

class C {
 public:
  C();
  virtual ~C();
  virtual void foo();
};
void bar() {
  new C();
}

C::C() {

}

C::~C() {

}

void C::foo() {
}

int main(void)
{
  return 0;
}
