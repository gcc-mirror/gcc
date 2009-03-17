// PR debug/39379
// { dg-do compile }
// { dg-options "-g -dA" }
// { dg-final { scan-assembler "DW_TAG_imported_module" }  }

namespace A
{
  int v;
}

int
main ()
{
  using namespace A;
  v++;
  return v - 1;
}
