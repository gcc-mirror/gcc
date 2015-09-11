/* { dg-do compile } */
/* { dg-options "-mshared -mexplicit-relocs -fno-delayed-branch -fno-unroll-loops" } */
/* We can load into something other than $25 when not optimizing,
   then immediately move into $25.  */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

void bar (void);

NOMIPS16 void
foo (int n)
{
  while (n--)
    {
      bar ();
      bar ();
    }
}

/* There should be exactly five uses of $25: one to set up $gp, two to
   load the address of bar (), and two to call it.  */
/* { dg-final { scan-assembler-times "\tl.\t\\\$25,%call16\\\(bar\\\)" 2 } } */
/* { dg-final { scan-assembler-times "\tjalrc?s?\t\\\$25" 2 } } */
/* { dg-final { scan-assembler "(\\\$28,|\t.cpload\t)\\\$25" } } */
/* { dg-final { scan-assembler-times "\\\$25" 5 } } */
