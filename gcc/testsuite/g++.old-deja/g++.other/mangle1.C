// Test for proper mangling by setting up name clashes.

#if (!defined (__GXX_ABI_VERSION) || __GXX_ABI_VERSION < 100)
#define NAME(OLD, NEW) OLD
#else
#define NAME(OLD, NEW) NEW
#endif /* (!defined (__GXX_ABI_VERSION) || __GXX_ABI_VERSION < 100) */

class A { };
typedef A A2;
typedef int I;
typedef void V;
typedef I I2;

void f (const A2&, int, const A2&, const A&) { } // ERROR - name clash
int NAME (f__FRC1AiT0T0, _Z1fRK1AiS1_S1_) = 0; // ERROR - name clash

void f (int, long, int, I) { } // ERROR - name clash
int NAME (f__Filii, _Z1filii) = 0; // ERROR - name clash

void f (I, float, I, I2) { } // ERROR - name clash
int NAME (f__Fifii, _Z1fifii) = 0; // ERROR - name clash

void f (void*, float, void*, V*) { } // ERROR - name clash
int NAME (f__FPvfT0T0, _Z1fPvfS_S_) = 0; // ERROR - name clash

void f (wchar_t) { } // ERROR - name clash
int NAME (f__Fw, _Z1fw) = 0; // ERROR - name clash

void f(int, A, A2, A) { } // ERROR - name clash
int NAME (f__FiG1AN21, _Z1fi1AS_S_) = 0; // ERROR - name clash

void f(const A2&, const A2&, const A2&, const A2&,
       int&) { } // ERROR - name clash
int NAME (f__FRC1AN30Ri, _Z1fRK1AS1_S1_S1_Ri) = 0; // ERROR - name clash

void f(const A2&, int, const A2&, const A2&, const A2&,
       int&) { } // ERROR - name clash
int NAME (f__FRC1AiT0N20Ri, _Z1fRK1AiS1_S1_S1_Ri) = 0; // ERROR - name clash

void f(const A2&, int, const A2&, const A2&, const A2&, int&, int&,
       int&) { } // ERROR - name clash
int NAME (f__FRC1AiT0N20RiN25, _Z1fRK1AiS1_S1_S1_RiS2_S2_) = 0; // ERROR - name clash

void f(const A2&, int, const A2&, const A2&, const A2&, int, int,
       int) { } // ERROR - name clash
int NAME (f__FRC1AiT0N20iii, _Z1fRK1AiS1_S1_S1_iii) = 0; // ERROR - name clash

void f(bool, bool) {} // ERROR - name clash
int NAME (f__FbT0, _Z1fbb) = 0; // ERROR - name clash

int
main ()
{
  return 0;
}

