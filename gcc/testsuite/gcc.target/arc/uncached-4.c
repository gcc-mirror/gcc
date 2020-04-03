/* { dg-do compile } */

#include <stddef.h>

typedef enum
{
 e1,
 e2
} my_enum_t;

typedef struct
{
  int a;
  int *p;
} my_struct_t;

typedef volatile struct
{
  my_enum_t a;
  my_struct_t b;
} __attribute__((uncached)) my_type_t;

my_type_t x;

void foo (my_type_t *p)
{
  p->a = e2;
  p->b.a = 10;
  p->b.p = NULL;
  *p->b.p = 10;
}

void bar (void)
{
  x.a = e2;
  x.b.a = 10;
  x.b.p = NULL;
  *x.b.p = 10;
}

/* { dg-final { scan-assembler-times "st\.di" 6 } } */
/* { dg-final { scan-assembler-times "ld\.di" 2 } } */
