// DR 2475 - Object declarations of type cv void
// { dg-do compile }

int f(), x;
extern void g(),
  y;                   // { dg-error "variable or field 'y' declared void" }
