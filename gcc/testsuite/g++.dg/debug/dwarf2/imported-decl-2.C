// { dg-do compile }
// { dg-options "-gdwarf-2 -gno-strict-dwarf -dA -O0 -fno-merge-debug-strings" }

class AAAA
{
 public:
  int method (void);
  int a;
};

int
AAAA::method (void)
{
  return a;
}

class BBBB : public AAAA
{
 public:
  using AAAA::method;

  int method (int b);
};

int
BBBB::method (int b)
{
  return a + b;
}

// { dg-final { scan-assembler-not "ascii \"BBBB\\\\0\".*ascii \"AAAA\\\\0\".*DW_TAG_imported_declaration" } }
// { dg-final { scan-assembler-times "ascii \"AAAA\\\\0\".*ascii \"BBBB\\\\0\".*DIE .0x\[0-9a-f\]*. DW_TAG_imported_declaration" 1 { xfail { powerpc-ibm-aix* } } } }
