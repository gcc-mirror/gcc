/* { dg-skip-if "Not supported in FDPIC" { arm*-*-uclinuxfdpiceabi } "*" "" } */
/* { dg-skip-if "-mpure-code and -fPIC incompatible" { *-*-* } { "-mpure-code" } } */
/* { dg-options "-fPIC -mno-pic-data-is-text-relative -mno-single-pic-base" } */
/* { dg-require-effective-target fpic } */
/* { dg-final { scan-assembler-not "j-\\(.LPIC"  } } */
/* { dg-final { scan-assembler "_GLOBAL_OFFSET_TABLE_-\\(.LPIC" } } */
/* { dg-final { scan-assembler "j\\(GOT\\)" } } */

static int j;

int *Foo ()
{
  return &j;
}
