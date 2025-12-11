/* { dg-do compile } */
/* { dg-options "-O2 -Wno-stringop-overflow" } */

extern void *memcpy (const void *, const void *, unsigned int);

void
test_memcpy (const void *p, const void *q)
{
  memcpy (p, q, 0x80000000);
}
