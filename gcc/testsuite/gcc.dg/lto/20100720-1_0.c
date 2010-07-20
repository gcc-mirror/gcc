/* { dg-lto-do run } */

struct X {
  int a;
};

typedef struct list_node *list;

struct list_node {
  list next;
  struct X *value;
};

list f(void)
{
  return 0;
}

int main(void)
{
  return 0;
}
