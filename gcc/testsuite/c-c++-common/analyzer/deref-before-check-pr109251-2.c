struct node
{
  struct node *next;
  int val;
};

int test_loop_1 (struct node *n)
{
  int total = 0;
  if (n->val = 42)
    return -1;
  for (struct node *iter = n; iter; iter=iter->next)
    total += iter->val;
  return total;
}

int test_loop_2 (struct node *n)
{
  int total = 0;
  if (n->val = 42)
    return -1;
  for (; n; n=n->next)
    total += n->val;
  return total;
}

#define FOR_EACH_NODE(ITER) for (; (ITER); (ITER)=(ITER)->next)

int test_loop_3 (struct node *n)
{
  int total = 0;
  if (n->val = 42)
    return -1;
  FOR_EACH_NODE (n)
    total += n->val;
  return total;
}
