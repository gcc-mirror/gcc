/* Test that we bump up low values of -mblock-move-inline-limit */
/* { dg-do compile } */
/* { dg-options "-O2 -mblock-move-inline-limit=8" } */

typedef __SIZE_TYPE__ size_t;
extern void *memcpy (void *, const void *, size_t);

void
cpy16 (void *x, void *y)
{
  memcpy (x, y, 16);
}

/* { dg-final { scan-assembler-not "memcpy" } } */
