// { dg-do assemble  }
// GROUPS niklas overloading
extern "C" void f (char*);
void f (const char*) {}
