// PR opt/13681
// Here we have an out-of-range array index.  We should not abort
// trying to resolve the indirection back to an object.

struct X { 
    double values[1]; 
    double & foo (const unsigned int index) { return values[index]; } 
}; 
 
void foo() { 
  double d; 
  X h1; 
  h1.foo(1) = d; 
} 
