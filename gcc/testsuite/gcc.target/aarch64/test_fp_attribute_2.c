/* { dg-do compile } */
/* { dg-options "-O0 -fomit-frame-pointer -fno-inline --save-temps" } */

void
leaf (void)
{
  int a = 0;
}

__attribute__ ((optimize("no-omit-frame-pointer")))
void
non_leaf_1 (void)
{
  leaf ();
}

__attribute__ ((optimize("no-omit-frame-pointer")))
void
non_leaf_2 (void)
{
  leaf ();
}

/* { dg-final { scan-assembler-times "stp\tx29, x30, \\\[sp, -\[0-9\]+\\\]!" 2 } } */

/* { dg-final { cleanup-saved-temps } } */
