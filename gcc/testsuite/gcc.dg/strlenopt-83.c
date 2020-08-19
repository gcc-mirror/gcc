/* PR tree-optimization/83821 - local aggregate initialization defeats
   strlen optimization
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-optimized" }
   { dg-require-effective-target alloca } */

#include "strlenopt.h"
char *p_p2, *p_p5, *p_p9, *p_p14;

unsigned n0, n1, n2, n3, n4;


static inline __attribute__ ((always_inline)) void
elim_strlen_of_consecutive_strcpy (char *p)
{
  p_p2 = p + 2;
  __builtin_strcpy (p_p2, "12");

  p_p5 = p_p2 + 3;
  __builtin_strcpy (p_p5, "124");

  p_p9 = p_p5 + 4;
  __builtin_strcpy (p_p9, "1245");

  p_p14 = p_p9 + 5;

  n0 = __builtin_strlen (p);
  n1 = __builtin_strlen (p_p2);
  n2 = __builtin_strlen (p_p5);
  n3 = __builtin_strlen (p_p9);

  /* The following isn't handled yet:
     n4 = __builtin_strlen (p_p14); */

  if (n0 || n1 != 2 || n2 != 3 || n3 != 4)
    __builtin_abort ();
}


void elim_strlen_of_consecutive_strcpy_in_alloca (unsigned n)
{
  /* Only known sizes are handled so far.  */
  n = 14;

  char *p = __builtin_alloca (n);

  *p = '\0';

  elim_strlen_of_consecutive_strcpy (p);
}


void elim_strlen_of_consecutive_strcpy_in_vla (unsigned n)
{
  /* Only known sizes are handled so far.  */
  n = 14;

  char vla[n];

  *vla = '\0';

  elim_strlen_of_consecutive_strcpy (vla);
}

void elim_strlen_of_consecutive_strcpy_in_malloc (unsigned n)
{
  char *p = __builtin_malloc (n);

  *p = '\0';

  elim_strlen_of_consecutive_strcpy (p);
}


void elim_strlen_of_consecutive_strcpy_in_calloc (unsigned n)
{
  char *p = __builtin_calloc (n, 1);

  /* Do not store into *P to verify that strlen knows it's zero.  */

  elim_strlen_of_consecutive_strcpy (p);
}

/* { dg-final { scan-tree-dump-not "abort" "optimized" } } */
