// Build don't link: 

struct B { struct A { A(); int a; } aa; };
struct A { A(); int a; };
B::A::A () { a = 37; }
char xx[]="../tests/m5.cc:3: Segmentation violation";
