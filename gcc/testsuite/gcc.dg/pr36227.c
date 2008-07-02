/* { dg-do compile } */
/* { dg-options "-O2 -Wstrict-overflow=3" } */
#if (__SIZEOF_LONG_LONG__ == __SIZEOF_POINTER__)
typedef unsigned long long ptrcast;
#elif (__SIZEOF_LONG__ == __SIZEOF_POINTER__)
typedef unsigned long ptrcast;
#elif (__SIZEOF_INT__ == __SIZEOF_POINTER__)
typedef unsigned int ptrcast;
#else
#error Add target support here
#endif

volatile unsigned long *
sat_add(volatile unsigned long *ptr, unsigned long i, volatile unsigned long *end)
{
  if ((ptrcast)ptr + i * sizeof(*ptr) > (ptrcast)ptr) /* { dg-bogus "pointer wraparound" } */
    return ptr + i;
  else
    return end;
}


