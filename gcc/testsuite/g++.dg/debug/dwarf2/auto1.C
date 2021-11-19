// PR c++/53756
// { dg-do compile { target c++14 } }
// { dg-options "-gdwarf-2 -dA -fno-debug-types-section -fno-inline" }
// We're looking for something like

// .uleb128 0x3    # (DIE (0x33) DW_TAG_subprogram)
// .ascii "a1\0"   # DW_AT_name
// .long   0x4c    # DW_AT_type
//...
// .uleb128 0x5    # (DIE (0x4c) DW_TAG_unspecified_type)
// .long   .LASF6  # DW_AT_name: "auto"
//...
// .uleb128 0x9    # (DIE (0x87) DW_TAG_base_type)
// .ascii "int\0"  # DW_AT_name
//...
// .uleb128 0x7    # (DIE (0x57) DW_TAG_subprogram)
// .long   0x33    # DW_AT_specification
// .long   0x87    # DW_AT_type

// { dg-final { scan-assembler "a1.*(0x\[0-9a-f]+)\[^\n\r]*DW_AT_type.*\\1. DW_TAG_unspecified_type.*(0x\[0-9a-f]+). DW_TAG_base_type.*DW_AT_specification\[\n\r]{1,2}\[^\n\r]*\\2\[^\n\r]*DW_AT_type" } }

struct A
{
  auto a1 () { return 42; }
};

int main()
{
  A a;
  a.a1();
}
