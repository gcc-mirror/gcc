// { dg-do assemble  }
// { dg-options "-Wno-pointer-arith" }
// 981203 bkoz
// g++/15054
// note that -pedantic also turns on this warning

void cuba(void) {
  void* p; 
  p++;
}
