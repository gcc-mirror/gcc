// { dg-options "-fshow-column -ansi -pedantic-errors -Wno-long-long" }
// PR c++/21908

struct virt { virt () {} virt (int i) {} };
struct der : public virtual virt { // { dg-error "34: note:                 der::der" }
  der (int i) : virt(i) {} // { dg-error "13: note: candidates are: der" }
};
struct top : public der { 
  top () {} // { dg-bogus "der\\(const" }
};
// { dg-error "10: error: no matching function for call to 'der" "" { target *-*-* } 9 }
