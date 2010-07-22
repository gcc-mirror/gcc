struct X {
  int b;
};

struct link {
  struct list_node *next;
};

struct list_node {
  struct link lnk;
  struct X *value;
};

void g(struct list_node *lst)
{
  lst->lnk.next = 0;
}
