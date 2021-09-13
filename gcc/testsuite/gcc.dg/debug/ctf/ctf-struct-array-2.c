/* CTF generation for struct type in presence of DWARF2.

   In case of DWARF2, the data member location is an expression containing
   the location.  CTF generation feeds off DWARF dies; this testcase tests
   that the location expr is handled.  */

/* { dg-do compile )  */
/* { dg-options "-O0 -gctf -dA -gdwarf-2" } */

/* { dg-final { scan-assembler-times "0x12000000\[\t \]+\[^\n\]*ctt_info" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x4\[\t \]+\[^\n\]*cta_nelems" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0\[\t \]+\[^\n\]*ctm_offset" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x20\[\t \]+\[^\n\]*ctm_offset" 1 } } */

static struct ranges {int from, to;} lim_regs[] = {{ 16, 7}, { 16, 6}, { 20, 7},{ 20, 6}};
