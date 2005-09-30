/* { dg-do compile } */
/* { dg-require-effective-target vect_condition } */

struct pred_data
{
  unsigned char codes[((int) 100)];
};

void compute_predicate_codes (char *codes, struct pred_data *p)
{
  int i;
  for (i = 0; i < ((int) 100); i++)
    codes[i] = p->codes[i] ? 2 : 0;
}

/* { dg-final { cleanup-tree-dump "vect" } } */
