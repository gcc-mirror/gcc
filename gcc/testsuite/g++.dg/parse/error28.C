// PR c++/21908

struct virt { virt () {} virt (int i) {} };
struct der : public virtual virt { // { dg-error "der" }
  der (int i) : virt(i) {} // { dg-error "der" }
};
struct top : public der { 
  // { dg-error "der\\(\\)" "" { target *-*-* } 9 } 
  top () {} // { dg-bogus "der\\(const" }
};
