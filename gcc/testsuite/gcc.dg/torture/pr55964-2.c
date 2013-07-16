/* { dg-do compile } */
/* { dg-options "-ftree-loop-distribution -funswitch-loops" } */

struct test_struct {
    int a, b[10], c[10], d[10];
};

extern struct test_struct* new_struct;

void testfunc(struct test_struct* old_struct)
{
  int i;
  for (i = 0; i < 10; ++i)
    {
      new_struct->b[i] = old_struct ? old_struct->b[i] : -1;
      new_struct->c[i] = old_struct ? old_struct->c[i] : 0;
      new_struct->d[i] = old_struct ? old_struct->d[i] : 0;
    }
  if (old_struct)
    old_struct->a++;
}
