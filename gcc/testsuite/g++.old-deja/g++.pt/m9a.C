// Build don't link: 

struct A { A() { a = 1; } int a; }; // ERROR - 
struct A { A() { a = 2; } int a; }; // ERROR - 
A aavv;
