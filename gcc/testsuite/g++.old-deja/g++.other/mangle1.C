// Test for proper mangling by setting up name clashes.

class A { };
typedef A A2;
typedef int I;
typedef void V;
typedef I I2;

void f (const A2&, int, const A2&, const A&) { } // ERROR - name clash
int _Z1fRK1AiS1_S1_ = 0; // ERROR - name clash

void f (int, long, int, I) { } // ERROR - name clash
int _Z1filii = 0; // ERROR - name clash

void f (I, float, I, I2) { } // ERROR - name clash
int _Z1fifii = 0; // ERROR - name clash

void f (void*, float, void*, V*) { } // ERROR - name clash
int _Z1fPvfS_S_ = 0; // ERROR - name clash

void f (wchar_t) { } // ERROR - name clash
int _Z1fw = 0; // ERROR - name clash

void f(int, A, A2, A) { } // ERROR - name clash
int _Z1fi1AS_S_ = 0; // ERROR - name clash

void f(const A2&, const A2&, const A2&, const A2&,
       int&) { } // ERROR - name clash
int _Z1fRK1AS1_S1_S1_Ri = 0; // ERROR - name clash

void f(const A2&, int, const A2&, const A2&, const A2&,
       int&) { } // ERROR - name clash
int _Z1fRK1AiS1_S1_S1_Ri = 0; // ERROR - name clash

void f(const A2&, int, const A2&, const A2&, const A2&, int&, int&,
       int&) { } // ERROR - name clash
int _Z1fRK1AiS1_S1_S1_RiS2_S2_ = 0; // ERROR - name clash

void f(const A2&, int, const A2&, const A2&, const A2&, int, int,
       int) { } // ERROR - name clash
int _Z1fRK1AiS1_S1_S1_iii = 0; // ERROR - name clash

void f(bool, bool) {} // ERROR - name clash
int _Z1fbb = 0; // ERROR - name clash

int
main ()
{
  return 0;
}

