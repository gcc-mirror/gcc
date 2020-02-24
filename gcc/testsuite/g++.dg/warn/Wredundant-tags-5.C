// PR c++/93804 - exempt extern "C" headers from -Wredundant-tags
// Verify that -Wredundant-tags is issued even for redundant class-key
// in references in the main source file to extern "C" classes defined
// in headers.
// { dg-do compile }
// { dg-options "-Wredundant-tags -ftrack-macro-expansion=0" }

#include "Wredundant-tags-5.h"

extern "C" {

  class C1                    // { dg-warning "\\\[-Wredundant-tags" }
  fc1 (C1)
  {
    return C1 ();
  }

  EC1
  fce1 (enum_class EC1)       // { dg-warning "\\\[-Wredundant-tags" }
  {
    return EC1 ();
  }

  E1
  fe1 (E1)
  {
    return (enum E1)0;        // { dg-warning "\\\[-Wredundant-tags" }
  }

  struct S1                   // { dg-warning "\\\[-Wredundant-tags" }
  fs1 (S1)
  {
    return S1 ();
  }

  U1
  fu1 (union U1)              // { dg-warning "\\\[-Wredundant-tags" }
  {
    return U1 ();
  }

}   // extern "C"


extern "C++" {

  class C2                    // { dg-warning "\\\[-Wredundant-tags" }
  fc2 (C2)
  {
    return C2 ();
  }

  EC2
  fce2 (enum_class EC2)       // { dg-warning "\\\[-Wredundant-tags" }
  {
    return EC2 ();
  }

  E2
  fe2 (E2)
  {
    return (enum E2)0;        // { dg-warning "\\\[-Wredundant-tags" }
  }

  struct S2                   // { dg-warning "\\\[-Wredundant-tags" }
  fs2 (S2)
  {
    return S2 ();
  }

  U2
  fu2 (union U2)              // { dg-warning "\\\[-Wredundant-tags" }
  {
    return U2 ();
  }

}   // extern "C++"


class C3                      // { dg-warning "\\\[-Wredundant-tags" }
fc3 (C3)
{
  return C3 ();
}

EC3
fce3 (enum_class EC3)         // { dg-warning "\\\[-Wredundant-tags" }
{
  return EC3 ();
}

E3 fe3 (E3)
{
  return (enum E3)0;          // { dg-warning "\\\[-Wredundant-tags" }
}

struct S3                      // { dg-warning "\\\[-Wredundant-tags" }
fs3 (S3)
{
  return S3 ();
}

U3
fu3 (union U3)                // { dg-warning "\\\[-Wredundant-tags" }
{
  return U3 ();
}

// { dg-prune-output "must not use the 'class' keyword" }
