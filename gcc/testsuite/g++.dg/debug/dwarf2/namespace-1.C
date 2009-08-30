// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR debug/41170
// { dg-options "-g -dA" }
//
// We want to test that there is a DW_TAG_namespace DIE DW_AT_name is set
// to "not_emitted". That namespace die has a child DW_TAG_typedef DIE
// which DW_AT_name is the null terminated string "T".
// { dg-final { scan-assembler-times "DIE +\\(.*?\\) DW_TAG_namespace" 1 } }
// { dg-final { scan-assembler-times "DW_AT_name: \"not_emitted\"" 1 } }
// { dg-final { scan-assembler-times "DIE +\\(.*?\\) DW_TAG_typedef" 1 } }
// { dg-final { scan-assembler-times "\.ascii \"T.0\"\[\t \]+.*?DW_AT_name" 1 } }

struct strukt
{
  int m;
};

namespace not_emitted
{
  typedef strukt T;
}

int
main()
{
  not_emitted::T t;
  t.m = 0;
  return 0;
}

