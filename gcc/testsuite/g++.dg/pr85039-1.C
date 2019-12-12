// { dg-do compile { target c++14 } }

constexpr int a() {
 return
  __builtin_offsetof(struct { // { dg-error "types may not be defined" }
    int i;
    short b {
      __builtin_offsetof(struct {
	int j;
        struct c {
          void d() {
          }
        };
      }, j)
    };
  }, i);
}
