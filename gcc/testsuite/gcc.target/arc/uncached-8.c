/* { dg-do compile } */

#include <stddef.h>

typedef struct
{
  int a;
} my_structB_t;

typedef struct
{
  my_structB_t b;
} __attribute__((uncached))  my_structA_t;

typedef volatile struct
{
  my_structA_t c;
} my_type_t;

my_type_t x;

void foo (my_type_t *p)
{
  p->c.b.a = 10;
}

void bar (void)
{
  x.c.b.a = 10;
}

/* { dg-final { scan-assembler-times "st\.di" 1 } } */
/* { dg-final { scan-assembler-times "st\.as\.di" 1 } } */
