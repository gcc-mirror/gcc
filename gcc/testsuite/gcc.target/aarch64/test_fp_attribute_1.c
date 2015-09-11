/* { dg-do compile } */
/* { dg-options "-O0 -fno-omit-frame-pointer -fno-inline --save-temps" } */

void
leaf (void)
{
  int a = 0;
}

__attribute__ ((optimize("omit-frame-pointer")))
void
non_leaf_1 (void)
{
  leaf ();
}

__attribute__ ((optimize("omit-frame-pointer")))
void
non_leaf_2 (void)
{
  leaf ();
}

/* { dg-final { scan-assembler-times "str\tx30, \\\[sp, -\[0-9\]+\\\]!" 2 } } */

