// DR 563

extern int i; // { dg-error "linkage" }
extern "C" int i; // { dg-error "linkage" }
