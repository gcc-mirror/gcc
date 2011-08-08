// { dg-do assemble  }
// { dg-options "" }
// PRMS Id: 4992

// { dg-prune-output "extended init" }

int *f(){
  return new int[1] = { 1 };   // { dg-error "lvalue" "err" }
}
