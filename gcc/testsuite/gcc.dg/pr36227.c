/* { dg-do compile } */
/* { dg-options "-O2 -Wstrict-overflow=3" } */

volatile unsigned long *
sat_add(volatile unsigned long *ptr, unsigned long i, volatile unsigned long *end)
{
  if ((unsigned long)ptr + i * sizeof(*ptr) > (unsigned long)ptr) /* { dg-bogus "pointer wraparound" } */
    return ptr + i;
  else
    return end;
}

