// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR debug/38390
// { dg-do compile  }
// { dg-options "-g -dA" }
// { dg-final { scan-assembler "DW_TAG_imported_module" }  }

namespace A
{
  int v;
}

int
f ()
{
  using namespace A;
  return v;
}

