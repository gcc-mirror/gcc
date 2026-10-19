// PR c++/103303
// { dg-do compile { target c++11 } }
// { dg-options "-O3 -msse2" }

struct alignas(16) A16 { virtual ~A16 () {} long l[2]; };
struct B8 { virtual ~B8 () {} };

struct S : virtual B8, virtual A16
{
  S ();
  long dummy;
  long v1;
  long v2;
};

// 'this' is only guaranteed to be 8-byte aligned in the base constructor.
S::S () : v1 (0), v2 (0) {}

// But a final class can't be a base subobject.
struct F final : virtual B8, virtual A16
{
  F ();
  long dummy;
  long v1;
  long v2;
};

F::F () : v1 (0), v2 (0) {}

// { dg-final { scan-assembler-times "movups\[^\n\r\]*16\\(%rdi\\)" 2 { target { ! ia32 } } } }
// { dg-final { scan-assembler-times "movaps\[^\n\r\]*16\\(%rdi\\)" 2 { target { ! ia32 } } } }
