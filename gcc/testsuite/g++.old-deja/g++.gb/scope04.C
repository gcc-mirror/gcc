// Build don't link: 
// GROUPS passed gb scope
struct a {
  struct c {
    struct d {
      static int foo (int);
    };
  };

  struct b {
    int foo (int x) { return c::d::foo (x); }
  };
};

int a::c::d::foo (int) { return 0; }
