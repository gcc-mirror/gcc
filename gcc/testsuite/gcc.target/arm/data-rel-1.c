/* { dg-options "-fPIC -mno-pic-data-is-text-relative" } */
/* { dg-final { scan-assembler-not "j-\\(.LPIC"  } } */
/* { dg-final { scan-assembler-not "_GLOBAL_OFFSET_TABLE_-\\(.LPIC" } } */
/* { dg-final { scan-assembler "j\\(GOT\\)" } } */
/* { dg-final { scan-assembler "(ldr|mov)\tr\[0-9\]+, \\\[?r9" } } */

static int j;

int *Foo ()
{
  return &j;
}
