/* { dg-do compile } */
/* { dg-options "-O2 -Wstrict-overflow=3" } */
typedef __UINTPTR_TYPE__ ptrcast;

volatile unsigned long *
sat_add(volatile unsigned long *ptr, unsigned long i, volatile unsigned long *end)
{
  if ((ptrcast)ptr + i * sizeof(*ptr) > (ptrcast)ptr) /* { dg-bogus "pointer wraparound" } */
    return ptr + i;
  else
    return end;
}


