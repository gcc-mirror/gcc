// PR c++/61804

struct T { void operator++(int); };
void f() { (T())++; }

struct U { void operator--(int); };
void g() { (U())--; }

void h() { int a; (int)++a; (int)--a; }
