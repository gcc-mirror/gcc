// Build don't link: 
// GROUPS passed gb scope
struct c {
  typedef int t;
  struct d {
    void foo (t &);
  };
};

void c::d::foo (t & x) { }
