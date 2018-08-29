/* { dg-do compile } */
/* { dg-additional-options "-fpeel-loops" } */

int
nj (int le)
{
  int zb;

  for (zb = 0; zb < 16; ++zb)
    le += le;

  return le * le;
}
