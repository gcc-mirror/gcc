// 981203 bkoz
// g++/15054
// Build don't link: 
// Special g++ Options: -Wno-pointer-arith
// note that -pedantic also turns on this warning

void cuba(void) {
  void* p; 
  p++;
}
