struct X {
  int b;
};

typedef struct list_node *list;

struct list_node {
  list next;
  list *ptr;
  struct X *value;
};

list *g(list *ptr)
{
  return ptr;
}
