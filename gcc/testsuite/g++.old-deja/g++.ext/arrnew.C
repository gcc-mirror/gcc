// { dg-do assemble  }
// { dg-options "" }
// PRMS Id: 4992

int *f(){
  return new int[1] = { 1 };   // { dg-error "" } removed
}
