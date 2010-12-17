/* { dg-lto-do run } */

struct X {
  int a;
};

typedef struct list_node *list;

struct list_node {
  list next;
  list *ptr;
  struct X *value;
};

list f(list lst)
{
  return lst->next;
}

int main(void)
{
  return 0;
}
