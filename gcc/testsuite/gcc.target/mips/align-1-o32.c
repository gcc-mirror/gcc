/* Check that typedef alignment does not affect passing of function
   parameters for O32 ABI.  */
/* { dg-do compile { target { "mips*-*-*" } } } */
/* { dg-options "-mabi=32"  } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */

typedef struct ui8
{
  unsigned v[8];
} uint8 __attribute__ ((aligned(64)));

unsigned
callee (int x, uint8 a)
{
  return a.v[0];
}

/* { dg-final { scan-assembler "\tsw\t\\\$5,1\\d\\d\\(\\\$(sp|fp)\\)" } } */
/* { dg-final { scan-assembler "\tsw\t\\\$6,1\\d\\d\\(\\\$(sp|fp)\\)" } } */
/* { dg-final { scan-assembler "\tsw\t\\\$7,1\\d\\d\\(\\\$(sp|fp)\\)" } } */
