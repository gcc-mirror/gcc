// Test for proper mangling by setting up name clashes.
// Special g++ Options: -fno-squangle

class A { };
typedef A A2;
typedef int I;
typedef void V;
typedef I I2;

void f (const A2&, int, const A2&, const A&) { } // ERROR - name clash
int f__FRC1AiT0T0 = 0; // ERROR - name clash

void f (int, long, int, I) { } // ERROR - name clash
int f__Filii = 0; // ERROR - name clash

void f (I, float, I, I2) { } // ERROR - name clash
int f__Fifii = 0; // ERROR - name clash

void f (void*, float, void*, V*) { } // ERROR - name clash
int f__FPvfT0T0 = 0; // ERROR - name clash

void f (wchar_t) { } // ERROR - name clash
int f__Fw = 0; // ERROR - name clash

void f(int, A, A2, A) { } // ERROR - name clash
int f__FiG1AN21 = 0; // ERROR - name clash

void f(const A2&, const A2&, const A2&, const A2&,
       int&) { } // ERROR - name clash
int f__FRC1AN30Ri = 0; // ERROR - name clash

void f(const A2&, int, const A2&, const A2&, const A2&,
       int&) { } // ERROR - name clash
int f__FRC1AiT0N20Ri = 0; // ERROR - name clash

void f(const A2&, int, const A2&, const A2&, const A2&, int&, int&,
       int&) { } // ERROR - name clash
int f__FRC1AiT0N20RiN25 = 0; // ERROR - name clash

void f(const A2&, int, const A2&, const A2&, const A2&, int, int,
       int) { } // ERROR - name clash
int f__FRC1AiT0N20iii = 0; // ERROR - name clash
