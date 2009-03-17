// PR debug/39379
// { dg-do compile }
// { dg-options "-g -dA" }
// { dg-final { scan-assembler "DW_TAG_imported_module" }  }

namespace A
{
  int v;
}

int
f ()
{
  int i;
  {
    using namespace A;
    v++;
    i = v - 1;
  }
  return i;
}
