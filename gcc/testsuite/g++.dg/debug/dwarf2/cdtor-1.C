// origin PR debug/49047
// { dg-options "-g -dA" }
// { dg-do compile }

struct K
{
  K () { }
  ~K () { }
};

int
main()
{
    K k;
}

// { dg-final {scan-assembler-times "\[^\n\r\]*DW_AT_MIPS_linkage_name:" 2 } }
