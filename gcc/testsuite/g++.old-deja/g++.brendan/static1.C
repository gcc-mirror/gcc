// Build don't link: 
// GROUPS passed static
class A { public: int a; };// ERROR - .*
void foo7 () { A::a = 3; }// ERROR - .*
