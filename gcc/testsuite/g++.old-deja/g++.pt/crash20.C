// Build don't link:

template <class T = int>
struct A { const T x; A() : x(0) { } A(T x) : x(x) { } }; 

template <class B>
void func () { B y; y = B(); } // ERROR - can't use default assignment

int main (void) { func< A<> >(); }
