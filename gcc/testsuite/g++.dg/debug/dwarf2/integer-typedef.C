// Origin: PR debug/49130
// { dg-options "-g -dA -fno-merge-debug-strings" }

typedef long unsigned int size_t;
static const size_t foo = 2048;

template<size_t size>
struct S
{
  void f(size_t);
};

template<size_t size>
inline void
S<size>::f(size_t)
{
  size_t i = size;
}

int
main()
{
  S<foo> s1;
  s1.f(10);
}

// { dg-final {scan-assembler-times "\"S<2048ul>.0\"\[^\n\r\]* DW_AT_name" 1 } }
// { dg-final {scan-assembler-times "\"_ZN1SILm2048EE1fEm.0\"\[^\n\r\]* DW_AT_\[MIPS_\]*linkage_name" 1 } }
