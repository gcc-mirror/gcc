struct X {
  char i;
};

struct W {
  struct U *p;
  struct X *q;
};

struct U {
  struct W a[1];
};

void bar(struct U *ptr)
{
  ptr->a[0].p = 0;
}
