/* PR debug/59350 */

/* { dg-do compile } */
/* { dg-options "-O -g " } */

typedef struct
{
  void *v;
  int len;
  int sign;
} ZVALUE;

extern int pred (ZVALUE);

static unsigned long
small_factor (ZVALUE z)
{
  if (z.len > 0)
    return 0;

  return pred (z) ? -1 : 0;
}

unsigned long
zfactor (ZVALUE z)
{
  z.sign = 0;
  return small_factor (z);
}
