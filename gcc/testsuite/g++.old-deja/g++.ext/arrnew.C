// { dg-do assemble  }
// { dg-options "" }
// PRMS Id: 4992

int *f(){
  return new int[1] = { 1 };   // { dg-error "lvalue" "err" }
  // { dg-warning "extended init" "warn" { target *-*-* } 6 }
}
