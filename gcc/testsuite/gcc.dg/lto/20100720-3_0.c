/* { dg-lto-do run } */

struct X {
  int a;
};

struct link {
  struct list_node *next;
};

struct list_node {
  struct link lnk;
  struct X *value;
};

void f(struct list_node *lst)
{
  lst->lnk.next = 0;
}

int main(void)
{
  return 0;
}
