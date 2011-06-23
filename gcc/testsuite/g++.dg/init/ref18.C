// PR c++/49395

volatile int foo();
struct A { volatile int i; };
typedef volatile int vi;

volatile int i;

const int& ir1 = foo();
//const int& ir2 = A().i;  // line 8
const int& ir3 = static_cast<volatile int>(i);
const int& ir4 = vi();  // line 10
