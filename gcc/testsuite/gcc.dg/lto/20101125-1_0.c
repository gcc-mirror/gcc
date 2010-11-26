/* { dg-lto-do link } */

struct X {
  int i;
};

struct W {
  struct U *p;
  struct X *q;
};

struct U {
  struct W a[1];
};

void foo(struct U *ptr)
{
  ptr->a[0].p = 0;
}

int main(void)
{
  return 0;
}
