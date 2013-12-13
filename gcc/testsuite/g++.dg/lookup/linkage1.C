// DR 563

extern int i; // { dg-message "linkage" }
extern "C" int i; // { dg-error "linkage" }
