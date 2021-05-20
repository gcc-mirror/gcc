/* Test Compilation of mixed constructs containing structs and arrays.

   Further, the compiler is expected to generate a single CTF struct type for
   struct cmodel (due to Type de-duplication at the time of CTF generation).
   
   const qualifier in fields of structs should be processed.  It appears as a
   no-name CTF record with appropriate ctt_info.  In this testcase, there are
   two const qualifiers - const char and const struct cmodel.  However, due to
   way the debug information is represented in DWARF die, 3 const qualifier
   records appear in the CTF section.

   <1><e1>: Abbrev Number: 14 (DW_TAG_typedef)
      <e2>   DW_AT_name        : (indirect string, offset: 0x114): cmodel_t
      <e9>   DW_AT_type        : <0x9a>
   <1><ed>: Abbrev Number: 13 (DW_TAG_const_type)
      <ee>   DW_AT_type        : <0xe1>
   <1><f2>: Abbrev Number: 4 (DW_TAG_array_type)
      <f3>   DW_AT_type        : <0xed>
      <f7>   DW_AT_sibling     : <0x102>

   <2><101>: Abbrev Number: 0
   <1><102>: Abbrev Number: 13 (DW_TAG_const_type)
      <103>   DW_AT_type        : <0xf2>
   <1><107>: Abbrev Number: 15 (DW_TAG_variable)
      <108>   DW_AT_name        : (indirect string, offset: 0x57): _models
      <10f>   DW_AT_type        : <0x102>
   <1><11d>: Abbrev Number: 0

   This results in:

   _models ->  e: const const cmodel_t [3] (size 0x30) -> d: const cmodel_t [3] (size 0x30)

   Deemed as acceptable for now.  */

/* { dg-do compile )  */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "ascii \"cmodel.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"cname.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"cpointer.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"cmodel_t.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */

/* 3 const records are expected.  */
/* { dg-final { scan-assembler-times "\[\t \]0x32000000\[\t \]+\[^\n\]*ctt_info" 3 } } */

struct a
{
  int a1[2];
  struct { int b[3]; } a2;
};

struct a my_a;

typedef struct cmodel
{
  const char *cname;
  int ccode;
  int cpointer;
} cmodel_t;

static const cmodel_t _models[] = {
  {"ILP32", 0, 4},
  {"LP64", 0, 8},
  {"", 0, 0}
};
