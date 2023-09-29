/* PR middle-end/28915 */
/* { dg-do compile } */
/* { dg-options "-msse -O2 -ftree-vectorize -fdump-tree-vect-optimized" } */

extern char lanip[3][40];
typedef struct
{
  char *t[8];
}tx_typ;

int set_names (void)
{
  __attribute__ ((used))
  static tx_typ tt1;
  int ln;
  for (ln = 0; ln < 8; ln++)
      tt1.t[ln] = lanip[1];
}

/* { dg-final { scan-tree-dump "optimized: loop vectorized" "vect" } } */
