/* { dg-do compile } */
/* { dg-options "-O2" } */

/* This case would fail to make use of the zero-overhead loop
   instruction at one time due to a bug.  */

extern char a[];

struct some_t
{
  struct
  {
    int aaa;
    short bbb;
    char ccc;
    char ddd;
  } ppp[8];

  int www[1];
};

int b;

void
some_function ()
{
  struct some_t *tmp = (struct some_t *) a;

  while ((*tmp).ppp[b].ccc)
    while(0);

  for (; b; b++)
    {
      if (tmp->ppp[b].ccc)
        {
          int c = tmp->ppp[b].bbb;
          int d = tmp->ppp[b].aaa;
          int e = d - tmp->www[c];
          if (e)
            tmp->ppp[b].ddd = 1;
        }
    }
}

/* { dg-final { scan-assembler "\[^\n\]+lp \\.L__GCC__" } } */
