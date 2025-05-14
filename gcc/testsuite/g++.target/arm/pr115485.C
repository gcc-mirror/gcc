/* { dg-do compile } */
/* { dg-options "-fPIE -mno-pic-data-is-text-relative -mlong-calls -ffunction-sections" } */

struct c1 {
  virtual void func1() = 0;
};
struct  c2 {
  virtual ~c2() {}
};
struct c3 : c2, c1 {
  void func1() override;
  void func3();
};
void c3::func1() {
  func3();
}
