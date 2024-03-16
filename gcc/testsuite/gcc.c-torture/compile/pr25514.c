struct node {
  struct node *next;
  int value;
};

struct node *current_node, global_list;

void foo (struct node *);

void
bar (void)
{
  struct node *node, *next;

  node = current_node;
  next = node->next;
  if (node != &global_list)
    current_node = next;
  else
    {
      node = global_list.next;
      global_list.value = node->value;
      global_list.next = node->next;
    }
  foo (node);
}
