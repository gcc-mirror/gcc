/* Test that we honor -mblock-move-inline-limit.  */
/* { dg-do compile } */
/* { dg-options "-O2 -mblock-move-inline-limit=128" } */

typedef __SIZE_TYPE__ size_t;
extern void *memcpy (void *, const void *, size_t);

void
cpy128 (void *x, void *y)
{
  memcpy (x, y, 128);
}

/* { dg-final { scan-assembler-not "memcpy" } } */
