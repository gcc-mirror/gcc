typedef struct x
{
  struct x *type;
  struct x *chain;
  struct x *value;
} *tree;

int
func (new, old)
     register tree new, old;
{
  if (old->type == 0 || new->type == 0)
    {
      register tree t = old->type;
      if (t == 0)
	t = new->type;
      for (; t; t = t->chain )
	if (t->value)
	  return 1;
    }
  return 0;
}
