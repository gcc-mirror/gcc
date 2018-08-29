// { dg-do compile { target c++14 } }

constexpr int a() {
 return
  __builtin_offsetof(struct { // { dg-error "types may not be defined" }
    int i;
    short b {
      __builtin_offsetof(struct { // { dg-error "types may not be defined" }
	int j;
        struct c { // { dg-error "types may not be defined" }
          void d() {
          }
        };
      }, j)
    };
  }, i);
}
