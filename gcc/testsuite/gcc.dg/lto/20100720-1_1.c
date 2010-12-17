struct X {
  int b;
};

typedef struct list_node *list;

struct list_node {
  list next;
  struct X *value;
};

list g(void)
{
  return 0;
}
