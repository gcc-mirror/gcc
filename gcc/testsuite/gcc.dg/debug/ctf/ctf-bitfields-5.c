/* Bitfield where the bit offset is > 255 is not allowed in CTF.

   PR debug/112878.
   This testcase is to ensure graceful handling. No slices are expected.  */

/* { dg-do compile { target bitint } } */
/* { dg-options "-O0 -gctf -dA" } */

/* No slices are expected, but a struct with one member is expected.
   CTF_K_UNKNOWN is also expected.  */
/* { dg-final { scan-assembler-times "cts_type" 0 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x1a000001\[\t \]+\[^\n\]*ctt_info" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"unknown.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */

struct {
  _BitInt(282) a : 280;
} b;
