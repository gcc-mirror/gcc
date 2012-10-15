// { dg-options "-std=c++11" }

struct B1 {
  B1(int) { }
};
struct B2 {
  B2(double) { }
};
struct D1 : B1 {    // { dg-error "no match" }
  using B1::B1;	    // implicitly declares D1(int)
  int x;
};
void test() {
  D1 d(6);	    // OK: d.x is not initialized
  D1 e;		    // { dg-error "deleted" } D1 has no default constructor
}
struct D2 : B2 {
  using B2::B2;	    // { dg-error "no match" } implicitly declares D2(double)
  B1 b;
};
D2 f(1.0);	    // { dg-error "deleted" } B1 has no default constructor
