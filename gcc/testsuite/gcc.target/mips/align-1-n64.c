/* Check that typedef alignment does not affect passing of function
   parameters for N64/N32 ABIs.  */
/* { dg-do compile { target { "mips*-*-*" } } } */
/* { dg-options "-mabi=64"  } */
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

/* { dg-final { scan-assembler "\tsd\t\\\$5,0\\(\\\$\[0-9\]\\)" } } */
/* { dg-final { scan-assembler "\tsd\t\\\$6,8\\(\\\$\[0-9\]\\)" } } */
/* { dg-final { scan-assembler "\tsd\t\\\$7,16\\(\\\$\[0-9\]\\)" } } */
