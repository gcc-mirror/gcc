// Build don't link:
// Special g++ Options: -Wall

const int& foo() {
  extern int bar;
  
  return bar;
}

const int* baz() {
  extern int bar;
  
  return &bar;
}
