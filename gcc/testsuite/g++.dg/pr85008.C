// PR c++/85008 ICE concerning dtor clones

void a() {
  struct b {
    ~b();
    int r [!!&b::~b]; // { dg-error "address of " }
  };
}
