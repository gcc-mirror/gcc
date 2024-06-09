// { dg-do compile }
// { dg-options "-Os -fnon-call-exceptions -mearly-ldp-fusion -fno-lifetime-dse -fno-forward-propagate" }
struct Class1 {
  virtual ~Class1() {}
  unsigned Field1;
};
struct Class4 : virtual Class1 {};
int main() { Class4 var1; }
