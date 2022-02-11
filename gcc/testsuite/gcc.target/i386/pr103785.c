/* { dg-do run } */
/* { dg-options "-O2" } */

#include <stdlib.h>

struct wrapper_t
{
  long k;
  long e;
};

struct wrapper_t **table;

__attribute__ ((weak, regparm (2)))
void
update (long k, long e)
{
  struct wrapper_t *elmt;

  elmt = table[k % 3079];
  if (elmt == 0)
    return;
  elmt->e = e;
}

int
main ()
{
  table = (struct wrapper_t **) malloc (20 * sizeof (struct wrapper_t *));
  for (int i = 0; i < 20; i++)
    table[i] = (struct wrapper_t *) calloc (sizeof (struct wrapper_t), 1);
  if (table[10]->e != 0)
    abort ();
  update (10, 20);
  if (table[10]->e != 20)
    abort ();
  return 0;
}
