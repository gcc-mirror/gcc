/* PR middle-end/79794  */
/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-final { scan-assembler-not "umov" } } */

struct node_struct
{
  float _Complex gap;
  unsigned long long state;
};

struct reg_struct
{
  int size;
  struct node_struct *node;
};

void
func(int target, struct reg_struct *reg)
{
  int i;

  for(i=0; i<reg->size; i++)
    reg->node[i].state ^= ((unsigned long long) 1 << target);
}
