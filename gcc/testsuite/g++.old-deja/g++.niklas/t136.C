// Build don't link: 
// GROUPS niklas overloading
extern "C" void f (char*);
void f (const char*) {}
