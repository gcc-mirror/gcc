/* { dg-options "-fPIC -mpic-data-is-text-relative" } */
/* { dg-final { scan-assembler "j-\\(.LPIC"  } } */
/* { dg-final { scan-assembler-not "_GLOBAL_OFFSET_TABLE_-\\(.LPIC" } } */
/* { dg-final { scan-assembler-not "j\\(GOT\\)" } } */

static int j;

int *Foo ()
{
  return &j;
}
