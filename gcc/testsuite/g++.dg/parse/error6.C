// PR c++/10603

int f(int not) { // { dg-error "!" }
  return 1-not; // { dg-error "" }
} 

