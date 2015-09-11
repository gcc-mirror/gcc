// { dg-do compile }

class basic_ostream {
public:
    basic_ostream &operator<<(basic_ostream &p1(basic_ostream &)) {
	return p1(*this);
    }
} a;
void fn1() __attribute__((__noreturn__));
basic_ostream &fn2(basic_ostream &) { fn1(); }
void fn3() { a << fn2; }
