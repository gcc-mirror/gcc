// { dg-do assemble  }
// { dg-options "-Wall" }

const int& foo() {
  extern int bar;
  
  return bar;
}

const int* baz() {
  extern int bar;
  
  return &bar;
}
