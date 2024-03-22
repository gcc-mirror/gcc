/* { dg-do compile } */
/* { dg-options "-O2 -mstrict-align" } */

#pragma GCC target "+nomops"

void
copy1 (int *x, int *y)
{
  __builtin_memmove (x, y, 12);
}

void
copy2 (int *x, int *y)
{
  __builtin_memmove (x, y, 128);
}

void
copy3 (int *x, int *y)
{
  __builtin_memmove (x, y, 255);
}

/* { dg-final { scan-assembler-times {\tb\tmemmove} 3 } } */
