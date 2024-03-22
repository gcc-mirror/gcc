// Testcase for mangling of unnamed types.

// namespace-scope unnamed types have no linkage, so we only test that they
// are distinct.

// { dg-do compile { target c++11 } }
// { dg-additional-options -fabi-compat-version=0 }

typedef struct { } *A;
typedef struct { } *B;

void f(A) { }
void f(B) { }

struct C
{
  typedef struct { }* D;
  typedef enum { e }* E;
};

// { dg-final { scan-assembler "_Z2g1PN1CUt_E" } }
void g1(C::D) { }
// { dg-final { scan-assembler "_Z2g2PN1CUt0_E" } }
void g2(C::E) { }

template <class T>
void h1(T t) { }

template <class T>
void h2(T t) { }

inline void j()
{
  typedef enum { f }* F;
// { dg-final { scan-assembler "_Z2h1IPZ1jvEUt_EvT_" } }
  h1(F());
  typedef struct { }* G;
// { dg-final { scan-assembler "_Z2h2IPZ1jvEUt0_EvT_" } }
  h2(G());
}

int main()
{
  j();
}
