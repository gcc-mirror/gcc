/* CTF generation for extern variable with defining and non-defining decl
   in the same CU.

   This testcase checks the case when a non-defining decl is followed by
   a defining decl for the same variable.  See PR debug/105089.
   
   In this testcase,  although two CTF array types are generated, only a
   single CTF variable and a single entry in the CTF object info section
   are expected.  */

/* { dg-do compile )  */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "0x12000000\[\t \]+\[^\n\]*ctt_info" 2 } } */

/* { dg-final { scan-assembler-times "\[\t \]0\[\t \]+\[^\n\]*cta_nelems" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x8\[\t \]+\[^\n\]*cta_nelems" 1 } } */
/* { dg-final { scan-assembler-times "ctv_name" 1 } } */
/* { dg-final { scan-assembler-times "objtinfo_var_type" 1 } } */

extern const char _CTF_NEWSTR[];
const char _CTF_NEWSTR[] = "ctfinfo"; 
