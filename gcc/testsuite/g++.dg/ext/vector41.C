// PR c++/100517
// { dg-options "" }

typedef int __v2si __attribute__ ((__vector_size__ (8)));

struct S { };

void
f (S s)
{
  (void) reinterpret_cast<__v2si> (s); // { dg-error "" }
}
