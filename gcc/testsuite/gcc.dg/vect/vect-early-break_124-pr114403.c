/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_long_long } */
/* { dg-additional-options "-mtune=generic-ooo" { target riscv*-*-* } } */

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */

#include "tree-vect.h"

typedef unsigned long PV;
typedef struct _buff_t {
    int foo;
    PV Val;
} buff_t;

#define NUM 9
#define SZ NUM * sizeof (PV)
char buffer[SZ];

__attribute__ ((noipa))
buff_t *copy (buff_t *first, buff_t *last)
{
  char *buffer_ptr = buffer;
  char *const buffer_end = &buffer[SZ-1];
  int store_size = sizeof(first->Val);
  while (first != last && (buffer_ptr + store_size) <= buffer_end)
    {
      const char *value_data = (const char *)(&first->Val);
      __builtin_memcpy(buffer_ptr, value_data, store_size);
      buffer_ptr += store_size;
      ++first;
    }

  if (first == last)
    return 0;

  return first;
}

int main ()
{

  check_vect ();

  /* Copy an ascii buffer.  We need to trigger the loop to exit from
     the condition where we have more data to copy but not enough space.
     For this test that means that OVL must be > SZ.  */
#define OVL NUM*2
  char str[OVL]="abcdefghiabcdefgh\0";
  buff_t tmp[OVL];

#pragma GCC novector
  for (int i = 0; i < OVL; i++)
    tmp[i].Val = str[i];

  buff_t *start = &tmp[0];
  buff_t *last = &tmp[OVL-1];
  buff_t *res = 0;

  /* This copy should exit on the early exit, in which case we know
     that start != last as we had more data to copy but the buffer
     was full.  */
  if (!(res = copy (start, last)))
    __builtin_abort ();

  /* Check if we have the right reduction value.  */
  if (res != &tmp[NUM-1])
    __builtin_abort ();

  int store_size = sizeof(PV);
#pragma GCC novector
  for (int i = 0; i < NUM - 1; i++)
    if (0 != __builtin_memcmp (buffer+(i*store_size), (char*)&tmp[i].Val, store_size))
      __builtin_abort ();

  return 0;
}
