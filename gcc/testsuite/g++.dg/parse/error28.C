// { dg-options "-fshow-column -ansi -pedantic-errors -Wno-long-long" }
// PR c++/21908

struct virt { virt () {} virt (int i) {} };
struct der : public virtual virt { // { dg-message "34:der::der" }
  der (int i) : virt(i) {} // { dg-message "3:candidates are: der" }
};
struct top : public der { 
  top () {} // { dg-bogus "der\\(const" }
};
// { dg-error "10:no matching function for call to 'der" "" { target *-*-* } 9 }
